(ns clojure-lwjgl.paint.core
  (:require [clojure-lwjgl.window :as window]
            [clojure-lwjgl.quad-buffer :as quad-buffer]
            [clojure-lwjgl.quad-list :as quad-list]
            [clojure-lwjgl.draw :as draw]
            [clojure-lwjgl.texture :as texture]))

(let  [initial-window (window/create)
       quad-buffer (quad-buffer/create)
       quad-list (quad-list/create)]
  (try
    (loop [window initial-window]
      (if (not @(:close-requested window))
        (recur (window/update window))
        (window/close window)))
    (catch Exception e
      (println e)
      (.printStackTrace e)
      (window/close initial-window))))
