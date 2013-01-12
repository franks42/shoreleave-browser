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


;;Why do I need the following declares to shut up the compiler about undeclared warnings???
(declare notify-kvmap-watches)
(declare add-kvmap-watch)
(declare remove-kvmap-watch)

(defprotocol IMutableKVMapWatchable
  "A mutable kvmap is a key-value store that lends itself to
  a map-like interface of one level deep.
  This protocol defines a watchers/pubsub interface,
  where watchers can be registered on the key as well as the whole store.
  Mimics the IWatchable interface with the addition of a map-key to identify 
  the key that should be watched."
  (notify-kvmap-watches [this map-key oldval newval] 
    "Notifies all registered watcher-fns of the changed value for map-key.")
  (add-kvmap-watch [this fn-key watcher-fn][this map-key fn-key watcher-fn]
    "Registers a watcher-fn for map-key value changes with a watch-fn id of fn-key.
    When value changes, the following is called:
    (watcher-fn fn-key this map-key oldval newval)
    When this kvmap is passed without a map-key, or if the map-key equals this kvmap,
    then the watcher-fn is registered on the kvmap itself, and will be notified 
    for every change of every key-value.")
  (remove-kvmap-watch [this][this map-key][this map-key fn-key]
    "Remove all the watcher-fns registered on all the map-keys of this kvmap,
    or remove all the watcher-fns registered on the map-key,
    or remove the single watcher-fn registered on the map-key with id fn-key.
    If map-key equals this kvmap then the watcher-fns registered on the kvmap itself are removed.")
    )

;; (declare notify-kvmap-watches)

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
  (-assoc! [kvm mapkey newvalue]
    (let [kvm-atm (.-kvmap-atom kvm)
          oldval (-lookup kvm mapkey)]
      (when-not (= oldval newvalue)
        (swap! kvm-atm assoc mapkey newvalue)
        (notify-kvmap-watches kvm mapkey oldval newvalue)))
    kvm)

  ITransientMap
  (-dissoc! [kvm mapkey]
    (let [kvm-atm (.-kvmap-atom kvm)
          oldval (-lookup kvm mapkey)]
      (when-not (nil? oldval)
        (swap! kvm-atm dissoc mapkey)
        ;; next is a hack to communicate the key to the notify-watches context
        ;; protocol doesn't really match well, but this way it "works"
        (notify-kvmap-watches kvm mapkey oldval nil)))
    kvm)

  IMutableKVMapWatchable
  (notify-kvmap-watches [kvm map-key oldval newval]
    (let [kvm-watchers-atm (.-kvmap-watchers-atom kvm)]
      (when-let [fns-map @kvm-watchers-atm]
        (doseq [k-f fns-map]
          ((val k-f) (key k-f) kvm map-key oldval newval))))
    (let [kvm-key-watchers-atm (.-kvmap-key-watchers-atom kvm)]
      (when-let [fns-map (get @kvm-key-watchers-atm map-key nil)]
        (doseq [k-f fns-map]
          ((val k-f) (key k-f) kvm map-key oldval newval))))
    kvm)
  (add-kvmap-watch 
    ([kvm fn-key f]
      (swap! (.-kvmap-watchers-atom kvm) assoc fn-key f)
      kvm)
    ([kvm map-key fn-key f]
      (swap! (.-kvmap-key-watchers-atom kvm) assoc-in [map-key fn-key] f)
      kvm))
  (remove-kvmap-watch
    ([kvm map-key fn-key]
      (if (= kvm map-key)
        (swap! (.-kvmap-watchers-atom kvm) dissoc fn-key)
        (let [fns-map (get-in @(.-kvmap-key-watchers-atom kvm) [map-key])
              new-fns-map (dissoc fns-map fn-key)]
          (if (empty? new-fns-map)
            (swap! (.-kvmap-key-watchers-atom kvm) dissoc map-key)
            (swap! (.-kvmap-key-watchers-atom kvm) assoc-in [map-key] new-fns-map))))
      kvm)
    ([kvm map-key]
      (if (= kvm map-key)
        (reset! (.-kvmap-watchers-atom kvm) {})
        (swap! (.-kvmap-key-watchers-atom kvm) dissoc map-key))
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
      (-lookup ls k nil))
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
    (let [oldval (get ls k nil)]
      (when-not (= oldval v)
        (.set ls (pr-str k) (pr-str v))
        ;; next is a hack to communicate the key to the notify-watches context
        ;; protocol doesn't really match well, but this way we can make it "work"
        (-notify-watches ls {:key k :value oldval} {:key k :value v})
        (notify-kvmap-watches ls k oldval v)))
    ls)

  ITransientMap
  (-dissoc! [ls k]
    (let [oldval (get ls k nil)]
      (when-not (nil? oldval)
        (.remove ls (pr-str k))
        ;; next is a hack to communicate the key to the notify-watches context
        ;; protocol doesn't really match well, but this way it "works"
        (-notify-watches ls {:key k :value oldval} {:key k :value nil})
        (notify-kvmap-watches ls k oldval nil)))
    ls)

  ;; ITransientCollection

  ;; ough... jumping thru hoops to fulfill the IWatchable protocol requirements
  ;; the storage lookup key is not the same as the IWatchable's fn-key...
  ;; TODO: need to be able to add multiple watchers per key
  IWatchable
  (-notify-watches [ls oldval newval]
    (let [map-key (:key oldval)]
      (when-let [fns-map (get @ls-watchers map-key nil)]
        (doseq [k-f fns-map]
          ;; pass the map-key instead of the map-ref, 
          ;; because the local storage is a well-known singleton
          ;; and the map-key is the only useful "ref" to what changed
          ((val k-f) (key k-f) map-key (:value oldval) (:value newval)))))
;;           ((val k-f) (key k-f) ls (:value oldval) (:value newval)))))
    ls)
  (-add-watch [ls [map-key fn-key] f]
    (swap! ls-watchers assoc-in [map-key fn-key] f))
  (-remove-watch [ls [map-key fn-key]]
    (let [fns-map (get-in @ls-watchers [map-key])
          new-fns-map (dissoc fns-map fn-key)]
      (if (empty? new-fns-map)
        (swap! ls-watchers dissoc map-key)
        (swap! ls-watchers assoc-in [map-key] new-fns-map))))

  
  IMutableKVMapWatchable
  
  (notify-kvmap-watches [ls map-key oldval newval]
    (when-let [fns-map @ls-kvmap-watchers-atom]
      (doseq [k-f fns-map]
        ((val k-f) (key k-f) ls map-key oldval newval)))
    (when-let [fns-map (get @ls-kvmap-key-watchers-atom map-key nil)]
      (doseq [k-f fns-map]
        ((val k-f) (key k-f) ls map-key oldval newval)))
    ls)
  (add-kvmap-watch 
    ([ls fn-key f]
      (swap! ls-kvmap-watchers-atom assoc fn-key f)
      ls)
    ([ls map-key fn-key f]
      (swap! ls-kvmap-key-watchers-atom assoc-in [map-key fn-key] f)
      ls))
  (remove-kvmap-watch
    ([ls map-key fn-key]
      (if (= ls map-key)
        (swap! ls-kvmap-watchers-atom dissoc fn-key)
        (let [fns-map (get-in @ls-kvmap-key-watchers-atom [map-key])
              new-fns-map (dissoc fns-map fn-key)]
          (if (empty? new-fns-map)
            (swap! ls-kvmap-key-watchers-atom dissoc map-key)
            (swap! ls-kvmap-key-watchers-atom assoc-in [map-key] new-fns-map))))
      ls)
    ([ls map-key]
      (if (= ls map-key)
        (reset! ls-kvmap-watchers-atom {})
        (swap! ls-kvmap-key-watchers-atom dissoc map-key))
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
