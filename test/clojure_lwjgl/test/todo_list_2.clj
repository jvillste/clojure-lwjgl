(ns clojure-lwjgl.test.todo-list-2
  (:require (clojure-lwjgl [window :as window]
                           [font :as font]
                           [vector-rectangle :as vector-rectangle]
                           [input :as input]
                           [logged-access :as logged-access]
                           [zipper-list :as zipper-list]
                           [layoutable :as layoutable])
            (clojure-lwjgl.command [text :as text]
                                   [image :as image]
                                   [command :as command]
                                   [translate :as translate]
                                   [scale :as scale]
                                   [push-modelview :as push-modelview]
                                   [pop-modelview :as pop-modelview])
            (clojure.java [shell :as shell]
                          [io :as io])
            [clojure-lwjgl.test.dataflow :as dataflow]
            (clojure [string :as string]
                     [set]))
  (:import [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader]
           [java.awt Color Font FontMetrics RenderingHints]
           [clojure_lwjgl.triangle_batch TriangleBatch]
           [java.io File]))


(defrecord ViewPartCall [id])

(defprotocol Element
  (drawing-commands [element width height])
  (preferred-width [element])
  (preferred-height [element]))

(extend ViewPartCall
  command/Command
  {:create-runner identity}
  command/CommandRunner
  {:delete identity
   :run identity}
  Element
  {:drawing-commands (fn [view-part-call width height] [view-part-call])
   :preferred-width (fn [view-part-call]  (dataflow/get-global-value (conj (:id view-part-call) :preferred-width)))
   :preferred-height (fn [view-part-call] (dataflow/get-global-value (conj (:id view-part-call) :preferred-height)))})

(defn draw-view-part [application-state view-part-id]
  (doseq [command-runner (get-in application-state [:view-part-command-runners view-part-id])]
    (if (instance? ViewPartCall command-runner)
      (draw-view-part application-state (:id command-runner))
      (command/run command-runner))))

(defn render [application-state]
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)

  (draw-view-part application-state [:root-view-part])

  application-state)

(defn key-pressed [keyboard-event key]
  (and (= (:key-code keyboard-event)
          key)
       (= (:type keyboard-event)
          :key-pressed)))

(defn handle-events [application]
  (let [unread-events (concat (input/unread-keyboard-events)
                              (input/unread-mouse-events))]
    (when (not (empty? unread-events))
      (doseq [event unread-events]
        (swap! application (fn [application-state]
                             ((:event-handler application-state)
                              application
                              (assoc application-state
                                :event-handled false)
                              event)))))))

(defn view-part-is-defined? [application-state view-part-id]
  (contains? (:view-part-ids application-state) view-part-id))

(defn undefine-view-part [application-state view-part-id]
  (dorun (map command/delete (get-in application-state [:view-part-command-runners view-part-id])))

  (update-in application-state [:view-part-ids] disj view-part-id))

(defn define-view-part [application-state view-part-id]
  (undefine-view-part application-state view-part-id)
  (let [drawing-commands (if (contains? application-state view-part-id)
                           (get application-state view-part-id)
                           [])
        application-state (reduce define-view-part
                                  application-state
                                  (->> (filter #(and (instance? ViewPartCall %)
                                                     (not (view-part-is-defined? application-state (:id %))))
                                               drawing-commands)
                                       (map :id)))]

    (-> application-state
        (assoc-in [:view-part-command-runners view-part-id] (command/command-runners-for-commands drawing-commands))
        (update-in [:view-part-ids] conj view-part-id))))

(defn update-view-part [application-state view-part-id]
  (if (contains? application-state view-part-id)
    (define-view-part application-state view-part-id)
    (undefine-view-part application-state view-part-id)))

(defn update-view [application]
  (when (not (empty? (dataflow/changes @application)))
    (let [application-state (swap! application #(-> %
                                                    (assoc :changes-to-be-processed (dataflow/changes %))
                                                    (dataflow/reset-changes)))

          changed-view-part-ids (filter #(view-part-is-defined? application-state %)
                                        (:changes-to-be-processed application-state))]
      (render (swap! application
                     (fn [application-state]
                       (reduce update-view-part application-state changed-view-part-ids)))))))

(defn update [application]
  (handle-events application)
  #_(swap! application (fn [application-state]
                         (dataflow/define-to application-state :time (System/nanoTime))))
  (update-view application)
  application)

(defn create-application [window event-handler root-element-constructor]
  (-> (dataflow/create)
      (assoc
          :window window
          :view-part-command-runners {}
          :view-part-ids #{}
          :event-handler event-handler)
      (dataflow/define-to
        :width  @(:width window)
        :height  @(:height window)
        [:root-view-part :element] root-element-constructor
        :root-view-part #(drawing-commands (dataflow/get-value :element)
                                           (dataflow/get-global-value :width)
                                           (dataflow/get-global-value :height)))
      (define-view-part [:root-view-part])
      (atom)))

(defn close-application [application]
  (window/request-close (:window application))
  application)


;; VIEW PARTS

(defrecord ViewPart [drawing-commands-generator preferred-width-generator preferred-height-generator]
  Element
  (drawing-commands [view-part width height] (drawing-commands-generator width height))
  (preferred-width [view-part] (preferred-width-generator))
  (preferred-height [view-part] (preferred-height-generator)))

(defn run-view-part-definitions [id definitions]
  (dorun (map (fn [[key value]]
                (dataflow/define [id :element key] value))
              (partition 2 definitions))))

(defn create-view-part [id]
  (let [absolute-element-path (dataflow/absolute-path [id :element])]
    (->ViewPart (fn [width height]
                  (dataflow/define [id] (fn []
                                          (drawing-commands (dataflow/get-global-value absolute-element-path)
                                                            width
                                                            height)))
                  [(->ViewPartCall (dataflow/absolute-path id))])
                (fn [] (preferred-width (dataflow/get-global-value absolute-element-path)))
                (fn [] (preferred-height (dataflow/get-global-value absolute-element-path))))))

(defmacro view-part [name arguments definitions root-element]
  (let [id (gensym "id")]
    `(defn ~name [~id ~@arguments]
       (run-view-part-definitions ~id ~definitions)
       (dataflow/define [~id :element] (fn []
                                         ~root-element))

       (create-view-part ~id))))


;; ELEMENTS

(defrecord Text [contents font color]
  Element
  (drawing-commands [text width height] [(text/create 0 0
                                                      contents
                                                      font
                                                      color)])
  (preferred-width [text] (font/width font contents))
  (preferred-height [text] (font/height font)))

(defrecord Rectangle [width height color]
  Element
  (drawing-commands [rectangle requested-width requested-height]
    [(vector-rectangle/rectangle 0
                                 0
                                 requested-width
                                 requested-height
                                 color)])

  (preferred-width [rectangle] width)
  (preferred-height [rectangle] height)

  Object
  (toString [_] (str "(->Rectangle " width " " height " " color ")")))

(defrecord Box [margin outer inner]
  Element
  (drawing-commands [box requested-width requested-height]
    (concat (drawing-commands outer
                              requested-width
                              requested-height)
            (apply translate/translate margin margin (drawing-commands inner
                                                                       (preferred-width inner)
                                                                       (preferred-height inner)))))

  (preferred-width [box] (+ (* 2 margin)
                            (preferred-width inner)))

  (preferred-height [box] (+ (* 2 margin)
                             (preferred-height inner))))

(defrecord VerticalStack [elements]
  Element
  (drawing-commands [vertical-stack
                     requested-width
                     requested-height] (let [width (apply max (conj (map preferred-width elements)
                                                                    0))]
                                         (vec (concat [(push-modelview/->PushModelview)]
                                                      (reduce (fn [commands element]
                                                                (concat commands
                                                                        (drawing-commands element
                                                                                          width
                                                                                          (preferred-height element))
                                                                        [(translate/->Translate 0 (preferred-height element))]))
                                                              []
                                                              elements)
                                                      [(pop-modelview/->PopModelview)]))))
  (preferred-height [vertical-stack] (reduce + (map preferred-height elements)))
  (preferred-width [vertical-stack] (apply max (conj (map preferred-width elements)
                                                     0)))
  Object
  (toString [_] (str "(->VerticalStack " elements ")")))

(defrecord Stack [elements]
  Element
  (drawing-commands [stack requested-width requested-height] (vec (mapcat (fn [element]
                                                                            (drawing-commands element
                                                                                              requested-width
                                                                                              requested-height))
                                                                          elements)))
  (preferred-width [stack]
    (apply max (conj (map preferred-width elements)
                     0)))
  (preferred-height [stack]
    (apply max (conj (map preferred-height elements)
                     0)))

  Object
  (toString [_] (str "(->Stack " elements ")")))

(defrecord Translation [x y element]
  Element
  (drawing-commands [translation requested-width requested-height] (vec (apply translate/translate
                                                                               x
                                                                               y
                                                                               (drawing-commands element
                                                                                                 (preferred-width element)
                                                                                                 (preferred-height element)))))
  (preferred-width [translation] (+ x (preferred-width element)))

  (preferred-height [translation] (+ y (preferred-height element)))


  Object
  (toString [_] (str "(->Transaltion " x " " y " " element ")")))


;; TODO-LIST

(defn add-item [application-state item-list index value]
  (let [new-id (rand-int 10000)]
    (-> application-state
        (dataflow/define-to (concat item-list [:items new-id]) value)
        (dataflow/define-to (concat item-list [:item-order]) (zipper-list/insert (get application-state (concat item-list [:item-order]))
                                                                                 new-id
                                                                                 index)))))

(defn remove-item [application-state item-list index]
  (let [id (get (apply vector (zipper-list/items (get application-state (concat item-list [:item-order]))))
                (get application-state (concat item-list [:selection])))]
    (-> application-state
        (dataflow/undefine (concat item-list [:items id]))
        (dataflow/apply-to-value  (concat item-list [:item-order])  #(zipper-list/remove % id)))))


(defn handle-editing-event [application application-state editor event]
  (cond
   (key-pressed event input/escape)
   (assoc (dataflow/apply-to-value application-state (concat editor [:editing]) not)
     :event-handled true)

   (key-pressed event input/left)
   (dataflow/apply-to-value application-state (concat editor [:cursor-position]) dec)

   (key-pressed event input/right)
   (dataflow/apply-to-value application-state (concat editor [:cursor-position]) inc)

   :default application-state))

(defn handle-editor-event [application application-state editor event]
  (cond

   (key-pressed event input/enter)
   (dataflow/apply-to-value application-state (concat editor [:editing]) not)

   :default (if (get application-state (concat editor [:editing]))
              (handle-editing-event application application-state editor event)
              application-state)))

(defn handle-item-list-view-event [application application-state item-list-view event]
  (cond

   (key-pressed event input/down)
   (dataflow/apply-to-value application-state (concat item-list-view [:selection]) inc)

   (key-pressed event input/up)
   (dataflow/apply-to-value application-state (concat item-list-view [:selection]) dec)

   (key-pressed event input/space)
   (add-item application-state item-list-view  (get application-state  (concat item-list-view [:selection])) "New item" )

   (key-pressed event input/backspace)
   (remove-item application-state item-list-view  (get application-state (concat item-list-view [:selection])))

   :default (handle-editor-event application
                                 application-state
                                 (concat item-list-view [(keyword (str "editor" (get application-state (concat item-list-view [:selection])))) :element])
                                 event)))

(defn handle-event [application application-state event]

  (let [application-state (handle-item-list-view-event application application-state [:root-view-part :element :item-list-view :element] event)]
    (if (not (:event-handled application-state))
      (cond
       (key-pressed event input/escape)
       (close-application application-state)

       :default application-state)

      application-state)))



(view-part cursor [width height]
           []
           (->Rectangle width
                        height
                        [1 #_(float (/ (mod (dataflow/get-global-value :time)
                                            1000000000)
                                       1000000000))
                         0 0 1]))


(view-part editor [value selected]
           [:selected selected
            :value value
            :edited-value value
            :editing false
            :cursor-position 0]

           (let [text (dataflow/get-value :value)
                 cursor-position (dataflow/get-value :cursor-position)
                 font (font/create "LiberationSans-Regular.ttf" 15)]
             (->Box 2
                    (->Rectangle 0
                                 0
                                 (if (dataflow/get-value :selected)
                                   [0 0 1 1]
                                   [1 1 1 1]))
                    (->Stack (concat (if (dataflow/get-value :editing)
                                       [(->Translation (font/width font (subs text 0 cursor-position))
                                                       0
                                                       (cursor :cursor
                                                               (font/width font (subs text
                                                                                      cursor-position
                                                                                      (+ cursor-position 1)))
                                                               (font/height font)))]
                                       [])
                                     [(->Text text
                                              font
                                              [0 0 0 1])])))))

(view-part item-list-view []
           [:selection 0
            :item-order (zipper-list/create)]

           (->VerticalStack (vec (doall (map-indexed (fn [index item-id]
                                                       (let [absolute-item-path (dataflow/absolute-path [:items item-id])
                                                             absolute-selection-path (dataflow/absolute-path [:selection])]
                                                         (editor (keyword (str "editor" index))
                                                                 #(dataflow/get-global-value absolute-item-path)
                                                                 #(= index
                                                                     (dataflow/get-global-value absolute-selection-path)))))
                                                     (zipper-list/items (dataflow/get-value :item-order)))))))

(view-part background []
           []
           (->Rectangle 0 0 [1 1 1 1]))


(defn item-view []
  (->Stack [(background :background)
            (item-list-view :item-list-view)]))

(defn create-todo-list [window]
  (println "Creating application")
  (let [application (create-application window handle-event item-view)]
    (swap! application (fn [application-state]
                         (let [item-list-view-element [:root-view-part :element :item-list-view :element]]
                           (-> application-state
                               (add-item item-list-view-element 0 "Foo")
                               (add-item item-list-view-element 0 "Bar")
                               (dataflow/print-dataflow)))))
    application))

(defn start []
  (window/start 700 500
                60
                create-todo-list
                update
                identity
                (fn [application width height]
                  (println "resize callback")
                  (swap! application
                         #(dataflow/define-to
                            %
                            :width width
                            :height height))
                  application)))

(comment
  (start))