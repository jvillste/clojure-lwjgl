(ns flow-gl.applications.cello
  (:require (flow-gl.gui [drawable :as drawable]
                         [layout :as layout]
                         [view :as view]
                         [application :as application])

            (flow-gl [dataflow :as dataflow]
                     [debug :as debug])
            (flow-gl.graphics [font :as font])
            [clojure-cello.pitch-detector :as pitch-detector])
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

(defn normalized-half-steps-from-c [half-steps-from-a]
  (mod (+ half-steps-from-a
          9)
       12))

(deftest normalized-half-steps-from-c-test
  (are [half-steps-from-a result] (= (normalized-half-steps-from-c half-steps-from-a)
                                     result)
       0 9
       12 0))

(defn major-scale-index [half-steps-from-a]
  (let [half-steps-from-c (normalized-half-steps-from-c half-steps-from-a)]
    (if (>= half-steps-from-c 0)
      (count (filter #(<= % half-steps-from-c) major-scale-notes)))))

(deftest major-scale-index-test
  (are [half-steps-from-a major-scale-index-result] (= (major-scale-index half-steps-from-a)
                                                       major-scale-index-result)
       0 0
       1 0))

(def guitar-range {:lowest-note -31
                   :highest-note 10})

(def cello-range {:lowest-note -34
                  :highest-note 0})

;; Pitch

(defn start-pitch-detector [state-atom]
  (pitch-detector/create (fn [pitch probability time-stamp progress]
                           (when (> probability 0.9)
                             (swap! state-atom
                                    dataflow/define-to :pitch pitch)))))

(defn stop-pitch-detector [state]
  (pitch-detector/stop (get state [:pitch-detector]))
  state)

;; View parts

(defn scale-line-y-coordinate [scale-height note lowest-note highest-note]
  (- scale-height
     (-> note
         note-frequency
         (linearize-frequency (note-frequency lowest-note) (note-frequency highest-note))
         (* scale-height))))

(defn scale-line-color [note]
  (if (major-scale-notes (mod (- note 3) 12))
    [0.0 0.0 0.0 1.0]
    [0.9 0.9 0.9 1.0]))

(defn scale []
  (layout/->Stack (vec (map (fn [note]
                              (assoc (drawable/->Rectangle (dataflow/get-global-value :width)
                                                           2
                                                           (scale-line-color note))
                                :x 30
                                :y (scale-line-y-coordinate (dataflow/get-global-value :height) note (dataflow/get-global-value :lowest-note) (dataflow/get-global-value :highest-note))))
                            (range (dataflow/get-global-value :lowest-note) (dataflow/get-global-value :highest-note))))))


(defn note-labels []
  (let [font (font/create "LiberationSans-Regular.ttf" 15)]
    (layout/->Absolute (for [note (range (dataflow/get-global-value :lowest-note) (dataflow/get-global-value :highest-note))]
                         (assoc (drawable/->Text (str (note-name note)
                                                      (note-octave note)))
                           :x 10
                           :y (- (scale-line-y-coordinate (dataflow/get-global-value :height) note (dataflow/get-global-value :lowest-note) (dataflow/get-global-value :highest-note))
                                 7))))))

(defn pitch-indicator []
  (if (> (dataflow/get-global-value :pitch)
         0)
    (let [y (- (dataflow/get-global-value :height)
               (* (linearize-frequency (dataflow/get-global-value :pitch)
                                       (note-frequency (dataflow/get-global-value :lowest-note))
                                       (note-frequency (dataflow/get-global-value :highest-note)))
                  (dataflow/get-global-value :height)))]

      (drawable/->Triangle [0 0 0 1]
                           50.0 (+ y 10.0)
                           100.0 y
                           50.0 (- y 10.0)))
    (drawable/->Empty)))

(defn cello []
  (layout/->Stack [#_(view/init-and-call :pitch-indicator pitch-indicator)
                   (view/init-and-call :scale scale)
                   #_(view/init-and-call :note-labels note-labels)]))

(defn handle-event [state view event]
  (println "handling " event)
  (if (= (:type event
                :close))
    (stop-pitch-detector state)
    state))

(defn initialize [state state-atom]
  (let [scale-range cello-range]
    (dataflow/define-to state
      :pitch 0
      :lowest-note (:lowest-note scale-range)
      :highest-note (:highest-note scale-range)
      :pitch-detector (start-pitch-detector (:state-atom state)))))

(defn start []
  (application/start cello
                     :initialize initialize))

(run-tests)

(comment
  (start)
  (debug/set-active-channels
   :view-definition
   :initialization
   :dataflow
   :events
   :view-update
   :default
   :render
   ))