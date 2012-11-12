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

(defn handle-events [application application-state]
  (let [unread-events (concat (input/unread-keyboard-events)
                              (input/unread-mouse-events))]
    (if (empty? unread-events)
      application-state
      (reduce (partial (:event-handler application-state) application) application-state unread-events))))

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
  (swap! application (partial handle-events application))
  #_(swap! application (fn [application-state]
                         (dataflow/define-to application-state :time (System/nanoTime))))
  (update-view application)
  application)

(defn create-application [window event-handler root-view-part]
  (-> (dataflow/create)
      (assoc
          :window window
          :view-part-command-runners {}
          :view-part-ids #{}
          :event-handler event-handler)
      (dataflow/define-to
        :width  @(:width window)
        :height  @(:height window)
        :root-view-part root-view-part)
      (define-view-part [:root-view-part])
      (atom)))

(defn close-application [application]
  (window/request-close (:window application))
  application)

;; TODO-LIST

(defn add-item [application-state index value]
  (let [new-id (rand-int 10000)]
    (-> application-state
        (dataflow/define-to [:items new-id] value)
        (dataflow/apply-to-value [:item-order] #(zipper-list/insert % new-id index)))))

(defn remove-item [application-state index]
  (let [id (get (apply vector (zipper-list/items (:item-order application-state)))
                (:selection application-state))]
    (-> application-state
        (dataflow/undefine [:items id])
        (dataflow/apply-to-value [:item-order] #(zipper-list/remove % id)))))

(defn handle-edit-event [application application-state event]
  (cond

   (key-pressed event input/escape)
   (dataflow/apply-to-value application-state :editing not)

   (key-pressed event input/enter)
   (dataflow/apply-to-value application-state :editing not)

   (key-pressed event input/left)
   (dataflow/apply-to-value application-state :cursor-position dec)

   (key-pressed event input/right)
   (dataflow/apply-to-value application-state :cursor-position inc)

   :default application-state))

(defn handle-event [application application-state event]
  (cond

   (:editing application-state)
   (handle-edit-event application application-state event)

   (key-pressed event input/down)
   (dataflow/apply-to-value application-state :selection inc)

   (key-pressed event input/up)
   (dataflow/apply-to-value application-state :selection dec)

   (key-pressed event input/space)
   (add-item application-state (:selection application-state) "New item" )

   (key-pressed event input/backspace)
   (remove-item application-state (:selection application-state))

   (key-pressed event input/enter)
   (-> application-state
       (dataflow/apply-to-value :editing not)
       (dataflow/define-to :cursor-position 0))

   (key-pressed event input/escape)
   (close-application application-state)

   :default application-state))

(defmacro definition [key value]
  `(dataflow/define [id ~key] ~value))

#_(defmacro view-part [name arguments definitions root-element]
    (let [id (gensym "id")]
      `(defn ~name [~id ~@arguments]
         ~@(map (fn [[key value]]
                  `(dataflow/define [~id ~key] ~value))
                (partition 2 definitions))
         (dataflow/define [~id] (fn []
                                  (let [root# ~root-element]
                                    (dataflow/define [:preferred-height] (preferred-height root#))
                                    (dataflow/define [:preferred-width] (preferred-width root#))
                                    (drawing-commands root#
                                                      (dataflow/get-value :width)
                                                      (dataflow/get-value :height)))))
         (->ViewPartCall (dataflow/absolute-path ~id)))))

(defrecord ViewPart [drawing-commands-generator preferred-width preferred-height]
  Element
  (drawing-commands [view-part width height] (drawing-commands-generator width height))
  (preferred-width [view-part] preferred-width)
  (preferred-height [view-part] preferred-height))

(defmacro view-part [name arguments definitions root-element]
  (let [id (gensym "id")]
    `(defn ~name [~id ~@arguments]
       ~@(map (fn [[key value]]
                `(dataflow/define [~id :element ~key] ~value))
              (partition 2 definitions))

       (dataflow/define [~id :element] (fn []
                                         ~root-element))

       (let [root# (dataflow/get-value [~id :element])]
         (->ViewPart (fn [width# height#]
                       (dataflow/define [~id] (fn []
                                                (drawing-commands root#
                                                                  width#
                                                                  height#)))
                       [(->ViewPartCall (dataflow/absolute-path ~id))])
                     (preferred-width root#)
                     (preferred-height root#))))))


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
  (drawing-commands [rectangle requested-width requested-height] [(vector-rectangle/rectangle 0
                                                                                              0
                                                                                              requested-width
                                                                                              requested-height
                                                                                              color)])

  (preferred-width [rectangle] width)
  (preferred-height [rectangle] height))

(defrecord Box [margin outer inner]
  Element
  (drawing-commands [box requested-width requested-height] (concat (drawing-commands outer
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
                                         (concat [(push-modelview/->PushModelview)]
                                                 (reduce (fn [commands element]
                                                           (concat commands
                                                                   (drawing-commands element
                                                                                     width
                                                                                     (preferred-height element))
                                                                   [(translate/->Translate 0 (preferred-height element))]))
                                                         []
                                                         elements)
                                                 [(pop-modelview/->PopModelview)])))
  (preferred-height [vertical-stack] (reduce + (map preferred-height elements)))
  (preferred-width [vertical-stack] (apply max (conj (map preferred-width elements)
                                                     0))))

(defrecord Stack [elements]
  Element
  (drawing-commands [stack requested-width requested-height] (println "Stack drawing commands")
    (mapcat (fn [element]
                                                                       (drawing-commands element
                                                                                         requested-width
                                                                                         requested-height))
                                                                     elements))
  (preferred-width [stack]
    (println "Stack preferred width")
    (apply max (conj (map preferred-width elements)
                                            0)))
  (preferred-height [stack]
    (println "Stack preferred height")
    (apply max (conj (map preferred-height elements)
                                             0))))


#_(defn cursor [id width height]
    (dataflow/define [id] (fn [] [(vector-rectangle/rectangle 0
                                                              0
                                                              width
                                                              height
                                                              [(float (/ (mod (dataflow/get-value :time)
                                                                              1000000000)
                                                                         1000000000)) 0 0 1])]))
    (->ViewPartCall (dataflow/absolute-path id)))

#_(defn editor-view [state]
    #_(println "running editor")
    (let [font (font/create "LiberationSans-Regular.ttf" 15)
          text (str (dataflow/get-value (conj state :text)))
          cursor-position (dataflow/get-value (conj state :cursor-position))
          margin 5]
      (flatten (vector (vector-rectangle/rectangle 0 0
                                                   (+ (* 2 margin)
                                                      (font/width font text))
                                                   (+ (* 2 margin)
                                                      (font/height font))
                                                   (if (dataflow/get-value (conj state :selected))
                                                     [0 0 1 1]
                                                     [0.9 0.9 1 1]))
                       (if (and (dataflow/get-value (conj state :selected))
                                (dataflow/get-value (conj state :editing)))
                         (translate/translate (+ margin
                                                 (font/width font (subs text 0 cursor-position)))
                                              margin
                                              (cursor (font/width font (subs text
                                                                             cursor-position
                                                                             (+ cursor-position 1)))
                                                      (font/height font)))
                         [])
                       (text/create 5 5
                                    text
                                    font
                                    [0.0 0.0 0.0 1.0])))))

#_(defn define-editor-state [application-state editor-state-path text]
    (let [font (font/create "LiberationSans-Regular.ttf" 15)
          margin 5]
      (dataflow/define application-state
        (conj editor-state-path :text) text
        (conj editor-state-path :cursor-position) 0
        (conj editor-state-path :editing) false
        (conj editor-state-path :preferred-width) (fn [] (+ (* 2 margin)
                                                            (font/width font
                                                                        (dataflow/get-value (conj editor-state-path :text)))))
        (conj editor-state-path :preferred-height) (+ (* 2 margin)
                                                      (font/height font))
        (conj editor-state-path :view) (fn [] (editor-view editor-state-path)))))

#_(view-part item-list-view []
             #_(println "running item-list")
             (dataflow/with-values [item-order selection]
               (flatten (map-indexed (fn [line-number item-index]
                                       (translate/translate 0 (* line-number 30)
                                                            (editor item-index
                                                                    (= selection
                                                                       line-number))))
                                     (zipper-list/items item-order)))))

(view-part editor [item-id index]
           [:selected #(= index
                          (dataflow/get-global-value :selection))]

           (->Box 2
                  (->Rectangle 0
                               0
                               (if (dataflow/get-value :selected)
                                 [0 0 1 1]
                                 [1 1 1 1]))
                  (->Text (dataflow/get-global-value [:items item-id])
                          (font/create "LiberationSans-Regular.ttf" 15)
                          [0 0 0 1])))

(view-part item-list-view []
             []
             (->VerticalStack (doall (map-indexed (fn [index item-id]
                                              (editor (keyword (str "editor" item-id))
                                                      item-id
                                                      index))
                                            (zipper-list/items (dataflow/get-global-value :item-order))))))




(view-part background []
           []
           (->Rectangle 0 0 [1 1 1 1]))

(defn item-view []
  (drawing-commands (->Stack [(background :background)
                              (item-list-view :item-list-view)])
                    (dataflow/get-global-value :width)
                    (dataflow/get-global-value :height)))

(defn create-todo-list [window]
  (println "Creating application")
  (let [application (create-application window handle-event item-view)]
    (swap! application (fn [application-state]
                         (-> application-state
                             (dataflow/define-to
                               :selection 0
                               :item-order (zipper-list/create))
                             (add-item 0 "Foo")
                             (add-item 0 "Bar")
                             (add-item 0 "FooBar")
                             (add-item 0 "FooBar3"))))
    application))

(defn start []
  (window/start 700 500
                10
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