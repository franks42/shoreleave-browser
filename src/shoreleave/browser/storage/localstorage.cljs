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
(def ls-map-watchers (atom {}))

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

(defprotocol ITransientMapWatchable
  (-notify-map-watches [this map-key oldval newval])
  (-add-map-watch [this map-key fn-key f])
  (-remove-map-watch [this map-key fn-key])
  (-remove-all-map-watch [this map-key]))


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
        ;; protocol doesn't really match well, but this way it "works"
        (-notify-watches ls {:key k :value oldval} {:key k :value v})))
    ls)

  ITransientMap
  (-dissoc! [ls k]
    (let [oldval (get ls k nil)]
      (when-not (nil? oldval)
        (.remove ls (pr-str k))
        ;; next is a hack to communicate the key to the notify-watches context
        ;; protocol doesn't really match well, but this way it "works"
        (-notify-watches ls {:key k :value oldval} {:key k :value nil})))
    ls)

  ;; ITransientCollection

  ;; ough... jumping thru hoops to fulfill the IWatchable protocol requirements
  ;; the storage lookup key is not the same as the IWatchable's fn-key...
  ;; TODO: need to be able to add multiple watchers per key
  IWatchable
  (-notify-watches [ls oldval newval]
    (when-let [f (get @ls-watchers (:key oldval) nil)]
      (f (:key oldval) ls (:value oldval) (:value newval)))
    ls)
  (-add-watch [ls key f]
    (swap! ls-watchers assoc key f))
  (-remove-watch [ls key]
    (swap! ls-watchers dissoc key))

  ITransientMapWatchable
  (-notify-map-watches [ls map-key oldval newval]
    (when-let [fns-map (get @ls-map-watchers map-key nil)]
      (doseq [k-f fns-map]
        ((:val k-f) (:key k-f) ls map-key oldval newval)))
    this)
  (-add-map-watch [ls map-key fn-key f]
    (swap! ls-map-watchers assoc-in [map-key fn-key] f))
  (-remove-map-watch [ls map-key fn-key]
    (let [fns-map (get-in @ls-map-watchers [map-key])
          new-fns-map (dissoc fns-map fn-key)]
      (swap! ls-map-watchers assoc-in [map-key] new-fns-map)))
  (-remove-all-map-watch [ls map-key]
    (swap! ls-map-watchers dissoc map-key))

  ;IPrintable
  ;(-pr-seq  [c opts]
   ; #_(let  [pr-pair  (fn  [keyval]  (pr-sequential pr-seq "" " " "" opts keyval))]
   ;   (pr-sequential pr-pair "{" ", " "}" opts c))
   ; (-pr-seq (-persistent! c) opts))
)

(defn empty!
  "Clear the localStorage"
  [ls]
  (.clear ls))

;; ###Usage
;; You'll typically do something like: `(def local-storage (localstorage/storage)`
(defn storage
  "Get the browser's localStorage"
  []
  (goog.storage.mechanism.HTML5LocalStorage.))

