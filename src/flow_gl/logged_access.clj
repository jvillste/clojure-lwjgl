(ns flow-gl.logged-access
  (:refer-clojure :exclude [get get-in assoc])
  (:require clojure.set))

(def ^:dynamic changes)

(def ^:dynamic reads)

(defmacro with-access-logging [& body]
  `(binding [changes (atom #{})
             reads (atom #{})]
     ~@body))

(defn add-access [log key]
  (swap! log (fn [log]
               (clojure.set/union log
                                  (hash-set key)))))

(defn add-change [key]
  (when (bound? (var changes))
    (add-access changes key)))

(defn add-read [key]
  (when (bound? (var reads))
    (add-access reads key)))


;;; PUBLIC

(defn assoc [target-map & key-values]
  (dorun (map add-change (map first (partition 2 key-values))) )
  (apply assoc-in target-map key-values))

(defn get [target-map key]
  (add-read key)
  (clojure.core/get target-map key))

(defn get-in [target-map path]
  (add-read path)
  (clojure.core/get-in target-map path))