(ns clojure-lwjgl.paint.core
  (:require [clojure-lwjgl.window :as window]))

(let  [initial-window (window/create)]
  (try
    (loop [window initial-window]
      (if (not @(:close-requested window))
        (recur (window/update window))
        (window/close window)))
    (catch Exception e
      (println e)
      (.printStackTrace e)
      (window/close initial-window))))
