(ns flow-gl.applications.todo-list
  (:require (flow-gl.opengl [window :as window])
            (flow-gl.gui [input :as input]
                         [drawable :as drawable]
                         [layout :as layout]
                         [view :as view]
                         [application :as application])

            (flow-gl.graphics [font :as font])

            (flow-gl.data [zipper-list :as zipper-list])

            (flow-gl [dataflow :as dataflow]
                     [debug :as debug])

            (clojure.contrib [profile :as profile])))


(defn add-item [state item-list index value]
  (let [new-id (rand-int 10000)]
    (-> state
        (dataflow/define-to (concat item-list [:items new-id]) value)
        (dataflow/define-to (concat item-list [:item-order]) (zipper-list/insert (get state (concat item-list [:item-order]))
                                                                                 new-id
                                                                                 index)))))

(defn remove-item [state item-list index]
  (let [id (get (apply vector (zipper-list/items (get state (concat item-list [:item-order]))))
                (get state (concat item-list [:selection])))]
    (-> state
        (dataflow/undefine (concat item-list [:items id]))
        (dataflow/apply-to-value  (concat item-list [:item-order])  #(zipper-list/remove % id)))))

(defn items [state item-view]
  (map (fn [item-id] (dataflow/property-from state item-view [:item-list-view :items item-id]))
       (zipper-list/items (dataflow/property-from state item-view [:item-list-view :item-order]))))

(defn handle-editing-event [state editor event]
  (cond
   (input/key-pressed? event input/escape)
   (-> state
       (dataflow/define-to (concat editor [:cursor-position]) 0)
       (dataflow/define-to (concat editor [:editing]) false)
       (dataflow/define-to (concat editor [:edited-value]) (get state (concat editor [:value])))
       (assoc :event-handled true))

   (input/key-pressed? event input/left)
   (dataflow/apply-to-value state (concat editor [:cursor-position]) dec)

   (input/key-pressed? event input/right)
   (dataflow/apply-to-value state (concat editor [:cursor-position]) inc)

   (not (nil? (:character event)))
   (-> state
       (dataflow/apply-to-value (concat editor [:edited-value]) (fn [edited-value]
                                                                  (-> (StringBuffer. edited-value)
                                                                      (.insert (get state (concat editor [:cursor-position])) (:character event))
                                                                      (.toString))))

       (dataflow/apply-to-value  (concat editor [:cursor-position]) inc)
       (assoc :event-handled true))



   :default state))

(defn handle-editor-event [state editor event]
  (cond

   (input/key-pressed? event input/enter)
   (if (dataflow/property-from state editor :editing)
     ((dataflow/property-from state editor :change-listener)
      (dataflow/define-to state
        (concat editor [:value]) (dataflow/property-from state editor :edited-value)
        (concat editor [:cursor-position]) 0
        (concat editor [:editing]) false)
      (dataflow/property-from state editor :edited-value))
     (dataflow/define-to state (concat editor [:editing]) true))

   :default (if (get state (concat editor [:editing]))
              (handle-editing-event state editor event)
              state)))


(defn cursor [editor font]
  (let [text (dataflow/property editor :edited-value)
        cursor-position (dataflow/property editor :cursor-position)
        width (font/width font (subs text
                                     cursor-position
                                     (+ cursor-position 1)))
        height (font/height font)]
    (layout/->Translation (font/width font (subs text 0 cursor-position))
                          0
                          (drawable/->Rectangle width
                                                height
                                                [1 #_(float (/ (mod (dataflow/get-global-value :time)
                                                                    1000000000)
                                                               1000000000))
                                                 0 0 1]))))


(defn editor [value selected change-listener]
  (let [font (font/create "LiberationSans-Regular.ttf" 15)
        editor-path (dataflow/absolute-path [])]
    (dataflow/initialize
     :selected selected
     :value value
     :edited-value value
     :editing false
     :cursor-position 0
     :change-listener (fn [] change-listener)
     :mouse-over false)

    (view/initialize-view-part :cursor #(cursor editor-path
                                                font))

    (let [text (if (dataflow/get-value :editing)
                 (dataflow/get-value :edited-value)
                 (dataflow/get-value :value))]
      (-> (layout/->Box 2
                        (drawable/->Rectangle 0
                                              0
                                              (if (dataflow/get-value :mouse-over)
                                                [0.8 0.8 0.8 1]
                                                (if (dataflow/get-value :selected)
                                                  [0 0 1 1]
                                                  [1 1 1 1])))

                        (layout/->Stack (concat (if (dataflow/get-value :editing)
                                                  [(view/call-view-part :cursor)]
                                                  [])
                                                [(drawable/->Text text
                                                                  font
                                                                  [0 0 0 1])])))

          (view/add-mouse-event-handler editor-path (fn [state event]
                                                      (case (:type event)
                                                        :mouse-entered (dataflow/define-property-to state editor-path :mouse-over true)
                                                        :mouse-left (dataflow/define-property-to state editor-path :mouse-over false)
                                                        state)))))))

(defn editor-id [item-id]
  [(keyword (str "editor-" item-id))])


(defn item-list-view []
  (let [item-list-view-path (dataflow/absolute-path [])]
    (dataflow/initialize
     :selection 0
     :item-order (zipper-list/create)
     :selected-item-id #(nth (zipper-list/items (dataflow/property item-list-view-path :item-order))
                             (dataflow/property item-list-view-path :selection)
                             nil))

    (layout/->VerticalStack (vec (map-indexed (fn [index item-id]
                                                (view/init-and-call (editor-id item-id)
                                                                    (fn []
                                                                      (-> (editor (dataflow/property item-list-view-path [:items item-id])

                                                                                  #(= item-id
                                                                                      (dataflow/property item-list-view-path :selected-item-id))

                                                                                  (fn [state new-value]
                                                                                    (dataflow/define-to
                                                                                      state
                                                                                      (concat item-list-view-path [:items item-id])
                                                                                      new-value)))

                                                                          (view/add-mouse-event-handler item-id (fn [state event]
                                                                                                                  (if (and (= (:type event)
                                                                                                                              :left-mouse-button-up)
                                                                                                                           (= (:event-handling-direction state) :up))
                                                                                                                    (dataflow/define-to state (concat item-list-view-path [:selection]) index)
                                                                                                                    state)))))))

                                              (zipper-list/items (dataflow/property item-list-view-path :item-order)))))))


(defn handle-item-list-view-event [state item-list-view event]
  (cond

   (input/key-pressed? event input/down)
   (dataflow/apply-to-value state (concat item-list-view [:selection]) inc)

   (input/key-pressed? event input/up)
   (dataflow/apply-to-value state (concat item-list-view [:selection]) dec)

   (input/key-pressed? event input/backspace)
   (remove-item state item-list-view  (get state (concat item-list-view [:selection])))

   :default (let [state (handle-editor-event state
                                                         (concat item-list-view (editor-id (dataflow/property-from state item-list-view :selected-item-id)))
                                                         event)]
              (if (not (:event-handled state))
                (cond
                 (input/key-pressed? event input/space)
                 (add-item state item-list-view  (get state  (concat item-list-view [:selection])) "New item" )

                 :default state)

                state))))


(defn background []
  (drawable/->Rectangle 0 0 [1 1 1 1]))

(defn fps []
  (drawable/->Text (str (float (dataflow/get-global-value :fps)))
                   (font/create "LiberationSans-Regular.ttf" 12)
                   [0 0 0 1]))

(defn status []
  (layout/->VerticalStack
   (concat [(view/init-and-call :fps fps)]
           (->> (concat [(str "x: " (dataflow/get-global-value :mouse-x) " y: " (dataflow/get-global-value :mouse-y))]
                        (dataflow/get-global-value [:status]))
                (filter (fn [message] (not (= message nil))))
                (map str)
                (map (fn [message]
                       (drawable/->Text (str message)
                                        (font/create "LiberationSans-Regular.ttf" 12)
                                        [0 0 0 1])))))))

(defn item-view []
  (layout/->Stack [(view/init-and-call :background background)
                   (view/init-and-call :item-list-view item-list-view)
                   #_(layout/->DockBottom (view/init-and-call :status2 status))]))

(defn handle-item-view-event [state item-view event]
  (let [state (if (= (:type event)
                                 :mouse-moved)
                            (dataflow/define-to state [:status] (get state :view-parts-under-mouse))
                            state)]

    (let [state (handle-item-list-view-event state (concat item-view [:item-list-view]) event)]
      (if (not (:event-handled state))
        (cond
         (input/key-pressed? event input/escape)
         (application/close state)

         :default state)

        state))))

(defn initialize [state state-atom]
  (let [item-list-view [:elements :item-list-view]]
    (-> state
        (dataflow/define-to [:status] [])
        ((fn [state]
           (reduce (fn [state index]
                     (add-item state item-list-view 0 (str "Foo" index)))
                   state
                   (range 2)))))))

(defn start []
  (application/start item-view
                     :initialize initialize
                     :handle-event handle-item-view-event))

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
   )

  (debug/set-active-channels))