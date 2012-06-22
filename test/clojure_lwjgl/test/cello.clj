(ns clojure-lwjgl.test.cello
  (:require (clojure-lwjgl [triangle-list :as triangle-list]
                           [window :as window])
            [clojure-cello.pitch-detector :as pitch-detector])
  (:import [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader])
  (:use clojure.test))


(defn note-frequency [half-steps-from-a]
  (* 440
     (Math/pow 1.059463094359
               half-steps-from-a)))

(defn linearize-frequency [frequency minimum maximum]
  (/ (- (Math/log frequency)
        (Math/log minimum))
     (- (Math/log maximum)
        (Math/log minimum))))

(defn start-pitch-detector [pitch-atom]
  (pitch-detector/create (fn [pitch probability time-stamp progress]
                           (when (> probability 0.9)
                             (reset! pitch-atom pitch)))))

(defn stop-pitch-detector [application]
  (pitch-detector/stop (application :pitch-detector))
  application)

(defn render [application]
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
  (GL11/glMatrixMode GL11/GL_MODELVIEW)
  (GL11/glLoadIdentity)

  (triangle-list/render (application :pitch-indicator))
  (triangle-list/render (application :scale))
  application)

(def blue (map float [0.0 0.0 1.0]))
(def red (map float [1.0 0.0 0.0]))
(def green (map float [0.0 1.0 0.0]))

(def major-scale-notes #{0 2 4})

(defn add-opacity [color opacity]
  (concat color [opacity]))

(defn single-color-triangle [coordinates color]
  {:coordinates (map float (apply concat coordinates))
   :colors (apply concat (repeat 3 (concat color [(float 1.0)])))})

(defn multi-color-triangle [coordinates colors]
  {:coordinates (map float (apply concat coordinates))
   :colors (map float (apply concat colors))})

(defn rectangle [x y width height color]
  [(single-color-triangle [[x y]
                           [x (+ y height)]
                           [(+ x width) y]]
                          color)

   (single-color-triangle [[x (+ y height)]
                           [(+ x width) (+ y height)]
                           [(+ x width) y]]
                          color)])

(deftest add-opacity-test
  (is (= (map #(add-opacity % 1.0) [red green blue])
         '((1.0 0.0 0.0 1.0) (0.0 1.0 0.0 1.0) (0.0 0.0 1.0 1.0)))))

(deftest rectangle-test
  (is (= (rectangle 10 10 20 20 blue)
         '[{:coordinates (10.0 10.0 10.0 30.0 30.0 10.0), :colors (0.0 0.0 1.0 1.0 0.0 0.0 1.0 1.0 0.0 0.0 1.0 1.0)}
           {:coordinates (10.0 30.0 30.0 30.0 30.0 10.0), :colors (0.0 0.0 1.0 1.0 0.0 0.0 1.0 1.0 0.0 0.0 1.0 1.0)}])))

(defn scale-line-y-coordinate [scale-height note]
  (-> note
      note-frequency
      (linearize-frequency 50 800)
      (* scale-height)))

(defn scale-line-color [note]
  (if (major-scale-notes (mod note 12))
    blue
    red))

(defn scale [width height]
  (apply concat (map (fn [note] (rectangle 0 (scale-line-y-coordinate height note) width 1 (scale-line-color note)))
                     (range -30 10))))

(defn update-pitch-indicator [application]
  (if (> @(:pitch-atom application)
         0)
    (let [y (* (linearize-frequency @(:pitch-atom application)
                                    50
                                    800)
               @(:height (:window application)))]
      (assoc application
        :pitch-indicator (triangle-list/update (application :pitch-indicator)
                                               0
                                               {:coordinates (map float [0.0 (+ y 50.0)
                                                                         100.0 y
                                                                         0.0 (- y 50.0)])
                                                :colors (map float [0.0 0.0 1.0 1.0
                                                                    0.0 0.0 1.0 1.0
                                                                    0.0 0.0 1.0 1.0])})))
    application))

(defn update [application]
  (-> application
      (update-pitch-indicator)
      (render)))

(defn create-application [window]
  (let [pitch-atom (atom -1)]
    {:window window
     :pitch-atom pitch-atom
     :pitch-detector (start-pitch-detector pitch-atom)
     :pitch-indicator (-> (triangle-list/create 1)
                          (triangle-list/update 0 {:coordinates (map float [0.0 0.0
                                                                            100.0 0.0
                                                                            100.0 100.0])
                                                   :colors (map float [0.0 0.0 1.0 1.0
                                                                       0.0 0.0 1.0 1.0
                                                                       0.0 0.0 1.0 1.0])}))
     :scale (let [scale-triangles (scale 700 500)]
              (-> (triangle-list/create (count scale-triangles))
                  (triangle-list/update-many 0 scale-triangles)))}))

(defn start []
  (window/start 700 500
                30
                create-application
                update
                stop-pitch-detector))

(run-tests)

(comment
  )