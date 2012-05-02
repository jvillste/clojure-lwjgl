(ns clojure-lwjgl.applications.traditional
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
            [clojure.contrib.dataflow :as dataflow])

  (:use midje.sweet)
  (:import [org.lwjgl.opengl GL11]))



;; miten välitetään component-manageri funktioille joilla lisätään komponentteja?
;; thread local binding
;; currytään bufferi funktiolle ennen kuin funktio annetaan sille funktiolle joka sitä kutsuu
;; state monad


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
  (println  "layout" (:selection gui))
  (-> gui
      (apply-to-visual-list visual-list/apply-to-visuals
                            #(layout/vertical-stack 5 5 %)
                            (:labels gui))
      (apply-to-visual-list visual-list/apply-to-visual
                            :selection-rectangle
                            #(assoc %
                               :x 0
                               :y (:y (visual-list/get-visual (:visual-list gui)
                                                              (nth (:labels gui)
                                                                   (:selection gui))))))))

(defn generate-id [] (keyword (str (rand-int 100000000))))


(defn add-visual [gui id visual x y]
  (apply-to-visual-list gui visual-list/add-visual id (layout/absolute-layout visual x y)))

(defn add-label [gui message]
  (let [id (generate-id)]
    (-> gui
        (update-in [:labels] #(conj %
                                    id))
        (add-visual id
                    (text/create message)
                    10
                    10))))

(defn render [gui]
  (-> gui
      (clear)
      (update-in [:visual-list] visual-list/draw)))

(defn create-gui [window]
  (-> {:window window
       :visual-list (visual-list/create)
       :labels []
       :selection 0}

      (add-visual :selection-rectangle
                  (rectangle/create {:red 0.5 :green 0.5 :blue 0.5 :alpha 1}
                                    100
                                    15
                                    10)
                  5 5)
      (add-label "Foo 1")
      (add-label "Foo 2")
      (add-label "Foo 3")
      (add-label "Foo 4")
      (add-label "Foo 5")
      (add-label "Foo 6")
      (layout)
      (render)))

(defn key-pressed [keyboard-event key]
  (and (= (:key-code keyboard-event)
          key)
       (= (:type keyboard-event)
          :key-pressed)))

;; text editing

(defn edit-text [text keyboard-event]
  (if (re-find #"\w" (str (:character keyboard-event)))
    (str text (:character keyboard-event))
    text))

(defn update-label [label keyboard-event]
  (-> label
      (update-in [:content] #(edit-text % keyboard-event))
      layout/set-preferred-size))

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


