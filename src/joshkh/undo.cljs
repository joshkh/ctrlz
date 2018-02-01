(ns joshkh.undo
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require
    [reagent.core :as reagent]
    [re-frame.core :as re-frame]
    [re-frame.db :refer [app-db]]))


;; -- Configuration ----------------------------------------------------------

(def ^:private config (atom {:max-undos 50 ;; Maximum number of undo states maintained
                             :harvest-fn deref
                             :post-reinstate-fn nil
                             :reinstate-fn reset!}))

(defn undo-config!
  "Set configuration parameters for library.

  Should be called on app startup."
  [new-config]
  (if-let [unknown-keys (seq (clojure.set/difference
                               (-> new-config keys set)
                               (-> @config keys set)))]
    (re-frame/console :error "re-frame-undo: undo-config! called within unknown keys: " unknown-keys)
    (swap! config merge new-config)))


(defn- max-undos
  []
  (:max-undos @config))



;; -- State history ----------------------------------------------------------

(def ^:private undo-list "A list of history states" (reagent/atom {}))
(def ^:private redo-list "A list of future states, caused by undoing" (reagent/atom {}))

;; -- Explanations -----------------------------------------------------------
;;
;; Each undo has an associated string explanation, for display to the user.
;;
;; It seems ugly to have mirrored vectors, but ...
;; the code kinda falls out when you do. I'm feeling lazy.
(def ^:private app-explain "Mirrors app-db" (reagent/atom ""))
(def ^:private undo-explain-list "Mirrors undo-list" (reagent/atom {}))
(def ^:private redo-explain-list "Mirrors redo-list" (reagent/atom {}))

(defn clear-undos!
  []
  (reset! undo-list {})
  (reset! undo-explain-list {}))


(defn clear-redos!
  []
  (reset! redo-list {})
  (reset! redo-explain-list {}))


(defn clear-history!
  []
  (clear-undos!)
  (clear-redos!)
  (reset! app-explain ""))


(defn store-now!
  "Stores the value currently in app-db, so the user can later undo"
  [explanation & [location]]
  (clear-redos!)
  (swap! undo-list update-in location (comp #(conj % ((:harvest-fn @config) app-db location)) vec (partial take-last (max-undos))))
  (swap! undo-explain-list update-in location (comp #(conj % explanation) vec (partial take-last (max-undos))))

  (reset! app-explain explanation))


(defn undos?
  "Returns true if undos exist, false otherwise"
  [location]
  (seq (get-in @undo-list location)))

(defn redos?
  "Returns true if redos exist, false otherwise"
  [location]
  (seq (get-in @redo-list location)))

(defn undo-explanations
  "Returns a vector of undo descriptions, perhaps empty"
  [location]
  (if (undos? location)
    (some-> @undo-explain-list (get-in location))
    []))

;; -- subscriptions  -----------------------------------------------------------------------------

(re-frame/reg-sub-raw
  :undos? ;;  usage:  (subscribe [:undos?])
  (fn handler
    ; "returns true if anything is stored in the undo list, otherwise false"
    [_ [_ location]]
    (reaction (undos? location))))

(re-frame/reg-sub-raw
  :redos?
  (fn handler
    ; "returns true if anything is stored in the redo list, otherwise false"
    [_ [_ location]]
    (reaction (redos? location))))


(re-frame/reg-sub-raw
  :undo-explanations
  (fn handler
    ; "returns a vector of string explanations ordered oldest to most recent"
    [_ [_ location]]
    (reaction (undo-explanations location))))

(re-frame/reg-sub-raw
  :redo-explanations
  (fn handler
    ; "returns a vector of string explanations ordered from most recent undo onward"
    [_ [_ location]]
    (reaction (get-in (deref redo-explain-list) location))))

;; -- event handlers  ----------------------------------------------------------------------------


(defn undo
  [harvester reinstater undos cur redos location]
  (let [u (get-in @undos location)
        r (cons (harvester cur location) (get-in @redos location))]
    (reinstater cur (last u) location)
    (swap! redos assoc-in location r)
    (swap! undos update-in location pop)))

(defn undo-n
  "undo n steps or until we run out of undos"
  [location n]
  (if-not (and (pos? n) (undos? location))
    (when-let [post-reinstate-fn (:post-reinstate-fn @config)] (post-reinstate-fn location))
    (do (undo (:harvest-fn @config) (:reinstate-fn @config) undo-list app-db redo-list location)
        (undo deref reset! undo-explain-list app-explain redo-explain-list location)
        (recur location (dec n)))))

(defn undo-handler
  [_ [_ location n]]
  (if-not (undos? location)
    (re-frame/console :warn "re-frame: you did a (dispatch [:undo]), but there is nothing to undo.")
    (undo-n location (or n 1)))
  {}) ; work is done directly on app-db

(defn redo
  [harvester reinstater undos cur redos location]
  (let [u (conj (get-in @undos location) (harvester cur location))
        r (get-in @redos location)]
    (reinstater cur (first r) location)
    (swap! redos assoc-in location (rest r))
    (swap! undos assoc-in location u)))

(defn redo-n
  "redo n steps or until we run out of redos"
  [location n]
  (if-not (and (pos? n) (redos? location))
    (when-let [post-reinstate-fn (:post-reinstate-fn @config)] (post-reinstate-fn location))
    (do
      (redo (:harvest-fn @config) (:reinstate-fn @config) undo-list app-db redo-list location)
      (redo deref reset! undo-explain-list app-explain redo-explain-list location)
      (recur location (dec n)))))

(defn redo-handler
  [_ [_ location n]] ;; if n absent, defaults to 1
  (if-not (redos? location)
    (re-frame/console :warn "re-frame: you did a (dispatch [:redo]), but there is nothing to redo.")
    (redo-n location (or n 1)))
  {}) ; work is done directly on app-db

(defn purge-redo-handler
  [db [_ location]]
  (if-not (redos? location)
    (re-frame/console :warn "re-frame: you did a (dispatch [:purge-redos]), but there is nothing to redo.")
    (clear-redos!))
  db)


;; -- Interceptors ----------------------------------------------------------

(defn undoable
  "returns a side-effecting Interceptor, which stores an undo checkpoint in
  `:after` processing.
   If the `:effect` cotnains an `:undo` key, then use the explanation provided
   by it. Otherwise, `explanation` can be:
     - a string (of explanation)
     - a function expected to return a string of explanation. It will be called
       with two arguments: `db` and `event-vec`.
     - a nil, in which case \"\" is recorded as the explanation
  "
  ([] (undoable nil))
  ([explanation]
   (re-frame/->interceptor
     :id :undoable
     :after (fn [context]
              (let [event (re-frame/get-coeffect context :event)
                    undo-effect (re-frame/get-effect context :undo)
                    {:keys [location message]} (cond
                                                 (some? undo-effect) undo-effect
                                                 (fn? explanation) (explanation
                                                                     (re-frame/get-coeffect context :db)
                                                                     event)
                                                 (string? explanation) explanation
                                                 (nil? explanation) ""
                                                 :else (re-frame/console :error "re-frame-undo: \"undoable\" interceptor on event " event " given a bad parameter. Got: " explanation))]
                (store-now! message location)
                (update context :effects dissoc :undo)))))) ;; remove any `:undo` effect. Already handled.


;; -- register handlers for events and subscriptions


(defn register-events-subs!
  []
  (re-frame/reg-event-fx
    :undo ;; usage:  (dispatch [:undo n])  n is optional, defaults to 1
    undo-handler)
  (re-frame/reg-event-fx
    :redo ;; usage:  (dispatch [:redo n])
    redo-handler)
  (re-frame/reg-event-db
    :purge-redos ;; usage:  (dispatch [:purge-redos])
    purge-redo-handler))

(register-events-subs!)
