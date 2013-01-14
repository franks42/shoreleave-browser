(ns shoreleave.browser.storage.localstorage
  "An idiomatic interface to the browser's local storage"
  (:require [cljs.reader :as reader]
            [goog.storage.mechanism.HTML5LocalStorage :as html5ls]))

;; Watchers
;; --------
;;
;; In most applications, you want to trigger actions when data is changed.
;; To support this, Shoreleave's local storage use IWatchable and maintains
;; the watchers in an atom.

(def ls-watchers (atom {}))

;; `localStorage` support
;; ----------------------
;;
;; For general information on localStorage, please see [Mozilla's docs](https://developer.mozilla.org/en/DOM/Storage#localStorage)
;;
;; Shoreleave's localStorage support is built against Closure's [interface](http://closure-library.googlecode.com/svn/docs/class_goog_storage_mechanism_HTML5LocalStorage.html)
;;
;; The extension supports the following calls:
;;
;;  * map-style lookup - `(:search-results local-storage "default value")`
;;  * `get` lookups
;;  * `(count local-storage)` - the number of things/keys stored
;;  * `(assoc! local-storage :new-key "saved")` - update or add an item
;;  * `(dissoc! local-storage :saved-results)` - remove an item
;;  * `(empty! local-storage)` - Clear out the localStorage store


;; define a no-value variable to distinguish between nil and no-value/non-existent
;; (def no-value :absolutely-non-existent-value)
(def no-value "uri:http://clojure.org/uri/no-value")

;; clj-js-string
(def clj-edn-prefix "clj-edn:")

(defn encode-edn-str [o]
  (if (string? o)
    o 
    (str clj-edn-prefix (pr-str o))))

(defn decode-edn-str [s]
  (let [n (count clj-edn-prefix)]
  (if (= (subs s 0 n) clj-edn-prefix)
    (cljs.reader/read-string (subs s n))
    s))

;;

(defprotocol IMutableKVMapWatchable
  "A mutable kvmap is a key-value store that lends itself to
  a map-like interface of one level deep.
  This protocol defines a watchers/pubsub interface,
  where watchers can be registered on the key as well as the whole store.
  Mimics the IWatchable interface with the addition of a mapkey to identify 
  the key that should be watched."
  (notify-kvmap-watches [this mapkey oldval newval] 
    "Notifies all registered watcher-fns of the changed value for mapkey.")
  (add-kvmap-watch [this fnkey watcher-fn][this mapkey fnkey watcher-fn]
    "Registers a watcher-fn for mapkey value changes with a watch-fn id of fnkey.
    When value changes, the following is called:
    (watcher-fn fnkey this mapkey oldval newval)
    When this kvmap is passed without a mapkey, or if the mapkey equals this kvmap,
    then the watcher-fn is registered on the kvmap itself, and will be notified 
    for every change of every key-value.")
  (remove-kvmap-watch [this][this mapkey][this mapkey fnkey]
    "Remove all the watcher-fns registered on all the mapkeys of this kvmap,
    or remove all the watcher-fns registered on the mapkey,
    or remove the single watcher-fn registered on the mapkey with id fnkey.
    If mapkey equals this kvmap then the watcher-fns registered on the kvmap itself are removed.")
    )


(deftype MutableKVMap [kvmap-atom kvmap-key-watchers-atom kvmap-watchers-atom])

(defn make-mutable-kvmap [] (MutableKVMap. (atom {}) (atom {}) (atom {})))

(extend-type MutableKVMap
  
  ILookup
  (-lookup
    ([kvm mapkey]
      (-lookup kvm mapkey nil))
    ([kvm mapkey not-found] 
      (let [kvm-atm (.-kvmap-atom kvm)]
        (get @kvm-atm mapkey not-found))))

  ICounted
  (-count [kvm] 
    (count (deref (.-kvmap-atom kvm))))

  IFn
  (-invoke
    ([kvm k]
      (-lookup kvm k))
    ([kvm k not-found]
      (-lookup kvm k not-found))) 

  ITransientAssociative
  (-assoc! [kvm mapkey newval]
    (let [kvm-atm (.-kvmap-atom kvm)
          oldval (-lookup kvm mapkey no-value)]
      (when-not (= oldval newval)
        (swap! kvm-atm assoc mapkey newval)
        (notify-kvmap-watches kvm mapkey oldval newval)))
    kvm)

  ITransientMap
  (-dissoc! [kvm mapkey]
    (let [kvm-atm (.-kvmap-atom kvm)
          oldval (-lookup kvm mapkey no-value)]
      (when-not (= oldval no-value)
        (swap! kvm-atm dissoc mapkey)
        ;; next is a hack to communicate the key to the notify-watches context
        ;; protocol doesn't really match well, but this way it "works"
        (notify-kvmap-watches kvm mapkey oldval no-value)))
    kvm)

  IMutableKVMapWatchable
  (notify-kvmap-watches [kvm mapkey oldval newval]
    (let [kvm-watchers-atm (.-kvmap-watchers-atom kvm)]
      (when-let [fns-map @kvm-watchers-atm]
        (doseq [k-f fns-map]
          ((val k-f) (key k-f) kvm mapkey oldval newval))))
    (let [kvm-key-watchers-atm (.-kvmap-key-watchers-atom kvm)]
      (when-let [fns-map (get @kvm-key-watchers-atm mapkey nil)]
        (doseq [k-f fns-map]
          ((val k-f) (key k-f) kvm mapkey oldval newval))))
    kvm)
  (add-kvmap-watch 
    ([kvm fnkey f]
      (swap! (.-kvmap-watchers-atom kvm) assoc fnkey f)
      kvm)
    ([kvm mapkey fnkey f]
      (swap! (.-kvmap-key-watchers-atom kvm) assoc-in [mapkey fnkey] f)
      kvm))
  (remove-kvmap-watch
    ([kvm mapkey fnkey]
      (if (= kvm mapkey)
        (swap! (.-kvmap-watchers-atom kvm) dissoc fnkey)
        (let [fns-map (get-in @(.-kvmap-key-watchers-atom kvm) [mapkey])
              new-fns-map (dissoc fns-map fnkey)]
          (if (empty? new-fns-map)
            (swap! (.-kvmap-key-watchers-atom kvm) dissoc mapkey)
            (swap! (.-kvmap-key-watchers-atom kvm) assoc-in [mapkey] new-fns-map))))
      kvm)
    ([kvm mapkey]
      (if (= kvm mapkey)
        (reset! (.-kvmap-watchers-atom kvm) {})
        (swap! (.-kvmap-key-watchers-atom kvm) dissoc mapkey))
      kvm)
    ([kvm]
      (reset! (.-kvmap-watchers-atom kvm) {})
      (reset! (.-kvmap-key-watchers-atom kvm) {})
      kvm))

  ;; deref'ing the kvmap gives you an immutable map instance to work with.
  ;; so does any get/lookup get you an immutable key-value
  IDeref
  (-deref [kvm] (deref (.-kvmap-atom kvm)))

  ;IPrintable
  ;(-pr-seq  [c opts]
   ; #_(let  [pr-pair  (fn  [keyval]  (pr-sequential pr-seq "" " " "" opts keyval))]
   ;   (pr-sequential pr-pair "{" ", " "}" opts c))
   ; (-pr-seq (-persistent! c) opts))
   
 ;; ITransientCollection


)

;;;;;;;;;;;;;;;;;;;;;;

(def ls-kvmap-watchers-atom (atom {}))
(def ls-kvmap-key-watchers-atom (atom {}))

(extend-type goog.storage.mechanism.HTML5LocalStorage
  
  ILookup
  (-lookup
    ([ls k]
      (-lookup ls k no-value))
    ([ls k not-found]
      (if-let [v (.get ls (pr-str k))]
        (cljs.reader/read-string v)
        not-found)))

  ICounted
  (-count [ls] 
    (.getCount ls))

  IFn
  (-invoke
    ([ls k]
      (-lookup ls k))
    ([ls k not-found]
      (-lookup ls k not-found))) 

  ITransientAssociative
  (-assoc! [ls k v]
    (let [oldval (get ls k no-value)]
      (when-not (= oldval v)
        (.set ls (pr-str k) (pr-str v))
;;         (-notify-watches ls {:key k :value oldval} {:key k :value v})
        (notify-kvmap-watches ls k oldval v)
        ))
    ls)

  ITransientMap
  (-dissoc! [ls k]
    (let [oldval (get ls k no-value)]
      (when-not (= oldval no-value)
        (.remove ls (pr-str k))
        ;; next is a hack to communicate the key to the notify-watches context
        ;; protocol doesn't really match well, but this way it "works"
;;         (-notify-watches ls {:key k :value oldval} {:key k :value nil})
        (notify-kvmap-watches ls k oldval no-value)))
    ls)

  ;; ITransientCollection

  ;; ough... jumping thru hoops to fulfill the IWatchable protocol requirements
  ;; the storage lookup key is not the same as the IWatchable's fnkey...
  ;; TODO: need to be able to add multiple watchers per key
  IWatchable
  (-notify-watches [ls oldval newval]
    (let [mapkey (:key oldval)]
      (when-let [fns-map (get @ls-watchers mapkey nil)]
        (doseq [k-f fns-map]
          ;; pass the mapkey instead of the map-ref, 
          ;; because the local storage is a well-known singleton
          ;; and the mapkey is the only useful "ref" to what changed
          ((val k-f) (key k-f) mapkey (:value oldval) (:value newval)))))
;;           ((val k-f) (key k-f) ls (:value oldval) (:value newval)))))
    ls)
  (-add-watch [ls [mapkey fnkey] f]
    (swap! ls-watchers assoc-in [mapkey fnkey] f))
  (-remove-watch [ls [mapkey fnkey]]
    (let [fns-map (get-in @ls-watchers [mapkey])
          new-fns-map (dissoc fns-map fnkey)]
      (if (empty? new-fns-map)
        (swap! ls-watchers dissoc mapkey)
        (swap! ls-watchers assoc-in [mapkey] new-fns-map))))

  
  IMutableKVMapWatchable
  
  (notify-kvmap-watches [ls mapkey oldval newval]
    (when-let [fns-map @ls-kvmap-watchers-atom]
      (doseq [k-f fns-map]
        ((val k-f) (key k-f) ls mapkey oldval newval)))
    (when-let [fns-map (get @ls-kvmap-key-watchers-atom mapkey nil)]
      (doseq [k-f fns-map]
        ((val k-f) (key k-f) ls mapkey oldval newval)))
    ls)
  (add-kvmap-watch 
    ([ls fnkey f]
      (swap! ls-kvmap-watchers-atom assoc fnkey f)
      ls)
    ([ls mapkey fnkey f]
      (swap! ls-kvmap-key-watchers-atom assoc-in [mapkey fnkey] f)
      ls))
  (remove-kvmap-watch
    ([ls mapkey fnkey]
      (if (= ls mapkey)
        (swap! ls-kvmap-watchers-atom dissoc fnkey)
        (let [fns-map (get-in @ls-kvmap-key-watchers-atom [mapkey])
              new-fns-map (dissoc fns-map fnkey)]
          (if (empty? new-fns-map)
            (swap! ls-kvmap-key-watchers-atom dissoc mapkey)
            (swap! ls-kvmap-key-watchers-atom assoc-in [mapkey] new-fns-map))))
      ls)
    ([ls mapkey]
      (if (= ls mapkey)
        (reset! ls-kvmap-watchers-atom {})
        (swap! ls-kvmap-key-watchers-atom dissoc mapkey))
      ls)
    ([ls]
      (reset! ls-kvmap-watchers-atom {})
      (reset! ls-kvmap-key-watchers-atom {})
      ls))

)

(defn empty!
  "Clear the localStorage"
  [ls]
  (.clear ls)
  ls)


;; The HTML5 Local Storage is a singleton, i.e. only one instance.
;; Not sure why Closure's goog.storage.mechanism.HTML5LocalStorage
;; constructor yields new variable instances for the same store (???)
;; the following tries to give you always the same var such that
;; you can actually compare them to be equal

(def local-storage (goog.storage.mechanism.HTML5LocalStorage.))

(defn get-local-storage
  "Get the browser's localStorage"
  [] local-storage)


(defn local-storage-keys 
  "Return the current list of keys of the local storage as a list of cljs-values.
  Note that from the moment that list is generated, it may be out-of-date
  as the local storage can be changed from other threads of work, 
  even from other browser windows."
  ([] (local-storage-keys (get-local-storage)))
  ([ls]
  (let [i (.__iterator__ ls true)] 
    (loop [lsks []] 
      (let [k (try (.next i) (catch js/Object e))]
        (if-not k
          lsks
          (recur (conj lsks (cljs.reader/read-string k)))))))))


;; window.addEventListener("storage", handle_storage, false);
;; function handle_storage(e) {
;;   if (!e) { e = window.event; }
;; }

(defn register-local-storage-event-watcher 
  "Register a 'storage' event handler that will notify the registered
  local-storage IMutableKVMapWatchable's watchers.
  The storage event will fire when the local storage is changed from
  within other windows, and could be used to communicate state between
  different windows served from the same domain."
  []
  (js/window.addEventListener 
    "storage" 
    (fn [e] 
  ;;     (println "storage event")
      (let [storage-area (.-storageArea e)
            local-storage? (= storage-area js/localStorage)]
        (when local-storage?
          (let [ls (get-local-storage)
                mapkey (cljs.reader/read-string (.-key e))
                oldValue (.-oldValue e)
                oldval (if oldValue (cljs.reader/read-string (.-oldValue e)) no-value)
                newval (cljs.reader/read-string (.-newValue e))]
;;         (println "\"storage\" event(mapkey, oldValue, newval, storageArea, local-storage?):" mapkey oldval newval storage-area local-storage?)
        (notify-kvmap-watches ls mapkey oldval newval)))))
    false))

(js/window.addEventListener 
  "storage" 
  (fn [e] 
    (println "storage event"))
  false)

(defn sync-mutable-kvmaps
  "Register watcher functions with the mutable key-value map 
  src-map to update and keep in sync dest-map.
  Single key-values can be sync'ed by specifying 
  src-map-key and dest-map-key.
  The whole src-map is sync'ed to dest-map by omitting any key info.
  Returns the registered watcher-fn or nil if anything wrong.
  The registered fn-key for the watcher-fn is the fn itself.
  src-map and dest-map should satisfy IMutableKVMapWatchable."
  ([src-map src-map-key dest-map dest-map-key]
    (when (and (satisfies? IMutableKVMapWatchable src-map)
               (satisfies? IMutableKVMapWatchable dest-map))
      (let [f (fn [fnkey this mapkey oldval newval]
                (if (= newval no-val)
                  (dissoc! dest-map dest-map-key)
                  (assoc! dest-map dest-map-key newval)))]
        (add-kvmap-watch src-map src-map-key f f)
        f)))
  ([src-map dest-map]
    (when (and (satisfies? IMutableKVMapWatchable src-map)
               (satisfies? IMutableKVMapWatchable dest-map))
      (let [f (fn [fnkey this mapkey oldval newval]
                (if (= newval no-val)
                  (dissoc! dest-map map-key)
                  (assoc! dest-map map-key newval)))]
        (add-kvmap-watch src-map f f)
        f))))
