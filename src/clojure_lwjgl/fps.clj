(ns clojure-lwjgl.fps)

(defn initialize [gui]
  (assoc gui
    :last-fps-print-time (System/nanoTime)
    :renders-since-fps-print 0))

(defn print [gui]
  (let [number-of-frames-to-skip 30
        print-at-this-frame (= number-of-frames-to-skip
                               (:renders-since-fps-print gui))]
    (when print-at-this-frame
      (println "FPS:" (/ 1
                         (/ (/ (- (System/nanoTime)
                                  (:last-fps-print-time gui))
                               1E9)
                            number-of-frames-to-skip))))

    (assoc gui
      :last-fps-print-time (if print-at-this-frame
                             (System/nanoTime)
                             (:last-fps-print-time gui))
      :renders-since-fps-print (if print-at-this-frame
                                 0
                                 (+ 1 (:renders-since-fps-print gui))))))