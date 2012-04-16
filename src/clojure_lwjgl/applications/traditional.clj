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
    :visual-list (reduce (fn [visual-list visual] (visual-list/update-visual visual-list (:id visual) visual))
                         (:visual-list gui)
                         visuals)))

(defn layout [gui]
  (let [labels (vertical-stack 5
                               5
                               (ids-to-visuals gui (:labels gui)))]
    (-> gui
        (update-in [:visual-list] #(visual-list/apply-to-visual %
                                                                :selection-rectangle
                                                                (fn [selection-rectangle]
                                                                  (assoc selection-rectangle
                                                                    :x 0
                                                                    :y (:y (nth labels (:selection gui)))))))
        (update-visuals labels))))

(defn generate-id [] (keyword (str (rand-int 100000000))))

(defn set-size [layoutable]
  (assoc layoutable
    :width (layoutable/preferred-width layoutable)
    :height (layoutable/preferred-height layoutable)))

(defn absolute-layout [layoutable x y]
  (-> layoutable
      set-size
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
      (add-label "Foo 6")))

(defn update-window [gui]
  (assoc gui :window (window/update (:window gui)
                                    30)))

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
  (visual-list/draw (:visual-list gui))
  (let [text (text/create "JWXY")]
    (-> (image-list/create)
        (image-list/add-image :text
                              0
                              400
                              (text/width text)
                              (text/height text))
        (image-list/draw-on-image :text #(text/render text %))
        (image-list/draw)
        (image-list/delete)))
  gui)

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
      set-size))

(defn handle-event [gui keyboard-event]
  (cond
   (key-pressed keyboard-event input/escape)
   (do (window/request-close (:window gui))
       gui)

   (key-pressed keyboard-event input/down)
   (assoc gui
     :selection (min (+ 1
                        (:selection gui))
                     (- (count (:labels gui))
                        1)))
   (key-pressed keyboard-event input/up)
   (assoc gui
     :selection (max (- (:selection gui)
                        1)
                     0))

   (re-find #"\w" (str (:character keyboard-event)))
   (update-in gui [:visual-list] (visual-list/apply-to-visual (nth (:labels gui) (:selection gui))
                                                              #(update-label % keyboard-event)))

   :default
   gui
   ))

(defn update-view [gui unread-keyboard-events]
  (-> (reduce handle-event gui unread-keyboard-events)
      layout))

(defn update [gui]
  (let [unread-keyboard-events (input/unread-keyboard-events)]
    (if (empty? unread-keyboard-events)
      (-> gui
          (clear)
          ;;          (update-view unread-keyboard-events)
          (render)
          (update-window))
      (-> gui
          (clear)
          (update-view unread-keyboard-events)
          (render)
          (update-window)))))


(defn start []
  (let [window (window/create 500 500)]
    (try
      (let [initial-gui (create-gui window)]
        (loop [gui initial-gui]
          (if (not @(:close-requested (:window gui)))
            (recur (update gui))
            (window/close window))))

      (catch Exception e
        (println e)
        (.printStackTrace e)
        (window/close window)))))

(comment
(start)
  )


