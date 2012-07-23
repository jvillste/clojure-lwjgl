(ns clojure-lwjgl.test.cello
  (:require (clojure-lwjgl [triangle-list :as triangle-list]
                           [window :as window]
                           [visual-list :as visual-list]
                           [text :as text])
            [clojure-cello.pitch-detector :as pitch-detector])
  (:import [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader])
  (:use clojure.test))

;; Music

(defn note-name [note]
  (["A" "A#" "B" "C" "C#" "D" "D#" "E" "F" "F#" "G" "G#"] (mod note 12)))

(defn note-octave [note]
  (+ 4
     (int (Math/floor (/ (+ note 10)
                         12)))))

(defn note-frequency [half-steps-from-a]
  (* 440
     (Math/pow 1.059463094359
               half-steps-from-a)))

(defn linearize-frequency [frequency minimum maximum]
  (/ (- (Math/log frequency)
        (Math/log minimum))
     (- (Math/log maximum)
        (Math/log minimum))))

(def major-scale-notes #{0 2 4 5 7 9 11})

(def guitar-range {:lowest-note -31
                   :highest-note 10})

(def cello-range {:lowest-note -34
                  :highest-note 0})

;; Pitch

(defn start-pitch-detector [pitch-atom]
  (pitch-detector/create (fn [pitch probability time-stamp progress]
                           (when (> probability 0.9)
                             (reset! pitch-atom pitch)))))

(defn stop-pitch-detector [application]
  (pitch-detector/stop (application :pitch-detector))
  application)

;; Graphics

(defn render [application]
  (let [scale 1]
    (GL11/glClearColor 1 1 1 0)
    (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
    (comment     (GL11/glMatrixMode GL11/GL_MODELVIEW)
                 (GL11/glLoadIdentity)
                 (GL11/glScalef scale (- scale) 1)
                 (GL11/glTranslatef 0 (- (* (/ 1 scale) @(:height (:window application)))) 0))
    )

  (triangle-list/render (application :pitch-indicator))
  (triangle-list/render (application :scale))
  (visual-list/draw (application :note-labels))

  application)

(def blue (map float [0.0 0.0 1.0]))
(def red (map float [1.0 0.0 0.0]))
(def green (map float [0.0 1.0 0.0]))

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


;; UI

(defn scale-line-y-coordinate [scale-height note lowest-note highest-note]
  (- scale-height
     (-> note
         note-frequency
         (linearize-frequency (note-frequency lowest-note) (note-frequency highest-note))
         (* scale-height))))

(defn scale-line-color [note]
  (if (major-scale-notes (mod (- note 3) 12))
    (map float [0.0 0.0 0.0])
    (map float [0.9 0.9 0.9])))

(defn scale [width height lowest-note highest-note]
  (apply concat (map (fn [note] (rectangle 30
                                           (scale-line-y-coordinate height note lowest-note highest-note)
                                           width
                                           1
                                           (scale-line-color note)))
                     (range lowest-note highest-note))))

(defn update-pitch-indicator [application]
  (if (> @(:pitch-atom application)
         0)
    (let [y (- @(:height (:window application))
               (* (linearize-frequency @(:pitch-atom application)
                                       (note-frequency (:lowest-note application))
                                       (note-frequency (:highest-note application)))
                  @(:height (:window application))))]
      (assoc application
        :pitch-indicator (triangle-list/update (application :pitch-indicator)
                                               0
                                               {:coordinates (map float [50.0 (+ y 10.0)
                                                                         100.0 y
                                                                         50.0 (- y 10.0)])
                                                :colors (map float [0.0 1.0 1.0 1.0
                                                                    0.0 1.0 0.0 1.0
                                                                    0.0 1.0 1.0 1.0])})))
    application))


(defn update [application]
  (-> application
      (update-pitch-indicator)
      (render)))

(defn create-note-labels [width height lowest-note highest-note]
  (loop [visual-list (visual-list/create)
         notes (range lowest-note highest-note)]
    (if (seq notes)
      (recur (visual-list/add-visual visual-list
                                     (first notes)
                                     (assoc (text/create (str (note-name (first notes))
                                                              (note-octave (first notes))))
                                       :x 10
                                       :y (- (scale-line-y-coordinate height (first notes) lowest-note highest-note)
                                             7)
                                       :width 50
                                       :height 50))
             (rest notes))
      visual-list)))


(defn create-application [window]
  (let [pitch-atom (atom -1)
        scale-range cello-range]
    {:lowest-note (:lowest-note scale-range)
     :highest-note (:highest-note scale-range)
     :window window
     :pitch-atom pitch-atom
     :pitch-detector (start-pitch-detector pitch-atom)
     :pitch-indicator (-> (triangle-list/create 1)
                          (triangle-list/update 0 {:coordinates (map float [0.0 0.0
                                                                            100.0 0.0
                                                                            100.0 100.0])
                                                   :colors (map float [0.0 0.0 1.0 1.0
                                                                       0.0 0.0 1.0 1.0
                                                                       0.0 0.0 1.0 1.0])}))
     :scale (let [scale-triangles (scale @(:width window) @(:height window)
                                         (:lowest-note scale-range) (:highest-note scale-range))]
              (-> (triangle-list/create (count scale-triangles))
                  (triangle-list/update-many 0 scale-triangles)))
     :note-labels (create-note-labels @(:width window) @(:height window)
                                      (:lowest-note scale-range) (:highest-note scale-range))}))

(defn start []
  (window/start 700 500
                15
                create-application
                update
                stop-pitch-detector))

(run-tests)
