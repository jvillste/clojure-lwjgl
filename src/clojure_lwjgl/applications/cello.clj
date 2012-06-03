(ns clojure-lwjgl.applications.cello
  (:refer-clojure :exclude (load))
  (:require (clojure-lwjgl [window :as window]
                           [visual :as visual]
                           [image-list :as image-list]
                           [visual-list :as visual-list]
                           [input :as input]
                           [group :as group]
                           [text :as text]
                           [free-layout :as free-layout]
                           [layoutable :as layoutable]
                           [rectangle :as rectangle]
                           [layout :as layout])
            [clojure-lwjgl.applications application]
            [clojure.zip :as zip]
            [clojure.contrib.dataflow :as dataflow]
            [clojure-cello.pitch-detector :as pitch-detector])

  (:use midje.sweet)
  (:import [org.lwjgl.opengl GL11]))

(defn start2 [] (pitch-detector/create (fn [pitch probability time-stamp progress] (println pitch))))
(defn stop [detector] (pitch-detector/stop detector))


(defn clear [gui]
  (let [scale 1]
    (GL11/glClearColor 1 1 1 0)
    (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
    (GL11/glMatrixMode GL11/GL_MODELVIEW)
    (GL11/glLoadIdentity)
    (GL11/glScalef scale (- scale) 1)
    (GL11/glTranslatef 0 (- (* (/ 1 scale) @(:height (:window gui)))) 0))
  gui)

(defn apply-to-visual-list [gui function & args]
  (assoc gui
    :visual-list (apply function (cons (:visual-list gui) args))))


(defn layout [gui]
  (-> gui
      (apply-to-visual-list visual-list/apply-to-visuals
                            #(layout/vertical-stack 5 5 %)
                            (:labels gui))
      (apply-to-visual-list visual-list/apply-to-visual
                            :selection-rectangle
                            #(let [selected-visual (visual-list/get-visual (:visual-list gui)
                                                                           (nth (:labels gui)
                                                                                (:selection gui)))]
                               (assoc %
                                 :x (:x selected-visual)
                                 :y (:y selected-visual)
                                 :width (:width selected-visual)
                                 :height (:height selected-visual))))))

(defn generate-id [] (keyword (str (rand-int 100000000))))


(defn add-visual [gui id visual x y]
  (apply-to-visual-list gui visual-list/add-visual id (layout/absolute-layout visual x y)))


(defn render [gui]
  (-> gui
      (clear)
      (update-in [:visual-list] visual-list/draw)))

(defn scale)

(defn create-gui [window]
  (-> {:window window
       :visual-list (visual-list/create)
       :played-frequency -1}

      (add-visual :selection-rectangle
                  (rectangle/create {:red 0.5 :green 0.5 :blue 0.5 :alpha 1}
                                    100
                                    15
                                    10)
                  5 5)
      (layout)
      (render)))

(defn key-pressed [keyboard-event key]
  (and (= (:key-code keyboard-event)
          key)
       (= (:type keyboard-event)
          :key-pressed)))


(defn handle-event [gui event]
  (cond
   (key-pressed event input/escape)
   (do (window/request-close (:window gui))
       gui)

   (key-pressed event input/down)
   (assoc gui
     :selection (min (+ 1
                        (:selection gui))
                     (- (count (:labels gui))
                        1)))

   (key-pressed event input/up)
   (assoc gui
     :selection (max (- (:selection gui)
                        1)
                     0))

   :default
   (apply-to-visual-list gui
                         visual-list/apply-to-visual
                         (nth (:labels gui)
                              (:selection gui))
                         #(update-label % event))))

(defn handle-events [gui events]
  (-> (reduce handle-event gui events)
      layout
      render))

(defn update [gui]
  (let [unread-events (concat (input/unread-keyboard-events)
                              (input/unread-mouse-events))]
    (if (empty? unread-events)
      gui
      (handle-events gui unread-events))))



(defn start []
  (window/start 500
                500
                20
                create-gui
                update))

(comment
(start)
  )
