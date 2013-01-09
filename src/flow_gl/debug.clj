(ns flow-gl.debug
  (:require [clojure.java.io :as io]))

;; DEBUG

(def log (atom []))
(def active-channels (atom #{:default}))

(defn set-active-channels [& channels]
  (reset! active-channels (into #{} channels)))

(defn reset-log []
  (reset! log []))

(defn debug [channel & messages]
  (when (contains? @active-channels channel)
    (swap! log conj (apply str messages)))
  (last messages))

(defn debug-all [channel messages]
  (doseq [message messages]
    (debug channel message)))

(defn debug-drop-last [channel & messages]
  (when (contains? @active-channels channel)
    (swap! log conj (apply str (drop-last messages))))
  (last messages))

(defn debug-if [channel condition & messages]
  (when (and (contains? @active-channels channel)
             (condition (last messages)))
      (swap! log conj (apply str messages)))
  (last messages))

(defn write-log []
  (with-open [writer (io/writer "debug-log.txt")]
    (doseq [line @log]
      (.write writer line)
      (.write writer "\n"))))


(defn debug-println [& values]
  (println (apply str values))
  (last values))