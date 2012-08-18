(ns clojure-lwjgl.logged-access)

(def ^:dynamic changes)

(def ^:dynamic reads)

(defmacro with-access-logging [& body]
  `(binding [changes (atom #{})
             reads (atom #{})]
     ~@body))

(defn add-access [log keys]
  (swap! log (fn [log]
               (clojure.set/union log
                                  (apply hash-set keys)))))

(defn add-changes [keys]
  (add-access changes keys))

(defn add-reads [keys]
  (add-access reads keys))

(defn logged-assoc [target-map & key-values]
  (add-changes (map first (partition 2 key-values)))
  (apply assoc target-map key-values))

(defn logged-get [target-map key]
  (add-reads [key])
  (get target-map key))

(comment (defn logged-update-in [m [k & ks] f & args]
           (update-in )))