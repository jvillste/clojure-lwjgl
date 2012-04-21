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
                           [rectangle :as rectangle])
            [clojure.zip :as zip]
            [clojure.contrib.dataflow :as dataflow])

  (:use midje.sweet)
  (:import [org.lwjgl.opengl GL11]))



;; miten välitetään component-manageri funktioille joilla lisätään komponentteja?
;; thread local binding
;; currytään bufferi funktiolle ennen kuin funktio annetaan sille funktiolle joka sitä kutsuu
;; state monad
(defn vertical-stack2 [x0 y0 visuals]
  (when (seq visuals)
    (let [visual (first visuals)]
      (println x0 " " y0)
      (recur x0
             (+ y0 (layoutable/preferred-height visual))
             (rest visuals)))))



(defn vertical-stack [x0 y0 visuals]
  (loop [visuals visuals
         layouted-visuals []
         y y0]
    (if (seq visuals)
      (let [visual (assoc (first visuals)
                     :y y
                     :x x0)]
        (recur (rest visuals)
               (conj layouted-visuals
                     visual)
               (+ y (layoutable/preferred-height visual))))
      layouted-visuals)))

(defrecord TestLayoutable [height]
  layoutable/Layoutable
  (layoutable/preferred-height [test-layoutable] (:height test-layoutable)))

(fact "vertical stack sets x and y coordinates"
  (vertical-stack 10
                  20
                  [(TestLayoutable. 10)
                   (TestLayoutable. 15)
                   (TestLayoutable. 10)])
  => [#clojure_lwjgl.applications.traditional.TestLayoutable{:height 10, :x 10, :y 20}
      #clojure_lwjgl.applications.traditional.TestLayoutable{:height 15, :x 10, :y 30}
      #clojure_lwjgl.applications.traditional.TestLayoutable{:height 10, :x 10, :y 45}])

(defn ids-to-visuals [gui ids]
  (reduce (fn [visuals visual-id] (conj visuals (assoc (visual-list/get-visual (:visual-list gui)
                                                                               visual-id)
                                                  :id visual-id)))
          []
          ids))

(defn update-visuals [gui visuals]
  (assoc gui
    :visual-list (reduce (fn [visual-list visual]
                           (visual-list/update-visual visual-list
                                                      (:id visual)
                                                      visual))
                         (:visual-list gui)
                         visuals)))

(defn apply-to-visual [gui function visual-id]
  (assoc gui :visual-list (visual-list/apply-to-visual (:visual-list gui)
                                                       visual-id
                                                       function)))

(defmacro thread-it [& [first-expr & rest-expr]]
  (if (empty? rest-expr)
    first-expr
    `(let [~'it ~first-expr]
       (thread-it ~@rest-expr))))

(defn apply-to-visuals [gui function visual-ids]
  (thread-it (ids-to-visuals gui visual-ids)
             (function it)
             (update-visuals gui it)))

(defn get-visual [gui visual-id]
  (visual-list/get-visual (:visual-list gui) visual-id))


(defn layout [gui]
  (-> gui
      (apply-to-visuals  #(vertical-stack 5 5 %)
                         (:labels gui))
      (apply-to-visual #(assoc %
                          :x 0
                          :y (:y (get-visual gui
                                             (nth (:labels gui)
                                                  (:selection gui)))))
                       :selection-rectangle)))

(defn generate-id [] (keyword (str (rand-int 100000000))))

(defn set-preferred-size [layoutable]
  (assoc layoutable
    :width (layoutable/preferred-width layoutable)
    :height (layoutable/preferred-height layoutable)))

(defn absolute-layout [layoutable x y]
  (-> layoutable
      set-preferred-size
      (assoc :x x
             :y y)))

(defn add-visual [gui id visual x y]
  (update-in gui [:visual-list] #(visual-list/add-visual %
                                                         id
                                                         (absolute-layout visual x y))))

(defn add-label [gui message]
  (let [id (generate-id)]
    (-> gui
        (update-in [:labels] #(conj %
                                    id))
        (add-visual id
                    (text/create message)
                    10
                    10))))



(defn clear [gui]
  (let [scale 1]
    (GL11/glClearColor 1 1 1 0)
    (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)
    (GL11/glMatrixMode GL11/GL_MODELVIEW)
    (GL11/glLoadIdentity)
    (GL11/glScalef scale (- scale) 1)
    (GL11/glTranslatef 0 (- (* (/ 1 scale) @(:height (:window gui)))) 0))
  gui)

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
      set-preferred-size))

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
   (apply-to-visual gui
                    #(update-label % event)
                    (nth (:labels gui) (:selection gui)))))

(defn update-view [gui unread-keyboard-events]
  (-> (reduce handle-event gui unread-keyboard-events)
      layout
      render))

(defn update [gui]
  (let [unread-keyboard-events (input/unread-keyboard-events)]
    (if (empty? unread-keyboard-events)
      (update-view gui unread-keyboard-events)
      (update-view gui unread-keyboard-events))))

(defn start []
  (window/start 500
                500
                20
                create-gui
                update))

(comment
  (start)
  )


