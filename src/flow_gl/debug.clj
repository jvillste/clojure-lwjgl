(ns flow-gl.debug
  (:require [clojure.java.io :as io]))

;; DEBUG

(def log (atom []))

(defn reset-log []
  (reset! log []))

(defn debug [& messages]
  (swap! log conj (apply str messages))
  (last messages))

(defn debug-all [messages]
  (doseq [message messages]
    (debug message)))

(defn debug-drop-last [& messages]
  (swap! log conj (apply str (drop-last messages)))
  (last messages))

(defn debug-if [condition & messages]
  (when (condition (last messages))
      (swap! log conj (apply str messages)))
  (last messages))

(defn write-log []
  (with-open [writer (io/writer "debug-log.txt")]
    (doseq [line @log]
      (.write writer line)
      (.write writer "\n"))))
