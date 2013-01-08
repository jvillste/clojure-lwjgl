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

(defn items [application-state item-view]
  (map (fn [item-id] (dataflow/property-from application-state item-view [:item-list-view :items item-id]))
       (zipper-list/items (dataflow/property-from application-state item-view [:item-list-view :item-order]))))

(defn handle-editing-event [application-state editor event]
  (cond
   (input/key-pressed? event input/escape)
   (-> application-state
       (dataflow/define-to (concat editor [:cursor-position]) 0)
       (dataflow/define-to (concat editor [:editing]) false)
       (dataflow/define-to (concat editor [:edited-value]) (get application-state (concat editor [:value])))
       (assoc :event-handled true))

   (input/key-pressed? event input/left)
   (dataflow/apply-to-value application-state (concat editor [:cursor-position]) dec)

   (input/key-pressed? event input/right)
   (dataflow/apply-to-value application-state (concat editor [:cursor-position]) inc)

   (not (nil? (:character event)))
   (-> application-state
       (dataflow/apply-to-value (concat editor [:edited-value]) (fn [edited-value]
                                                                  (-> (StringBuffer. edited-value)
                                                                      (.insert (get application-state (concat editor [:cursor-position])) (:character event))
                                                                      (.toString))))

       (dataflow/apply-to-value  (concat editor [:cursor-position]) inc)
       (assoc :event-handled true))



   :default application-state))

(defn handle-editor-event [application-state editor event]
  (cond

   (input/key-pressed? event input/enter)
   (if (dataflow/property-from application-state editor :editing)
     ((dataflow/property-from application-state editor :change-listener)
      (dataflow/define-to application-state
        (concat editor [:value]) (dataflow/property-from application-state editor :edited-value)
        (concat editor [:cursor-position]) 0
        (concat editor [:editing]) false)
      (dataflow/property-from application-state editor :edited-value))
     (dataflow/define-to application-state (concat editor [:editing]) true))

   :default (if (get application-state (concat editor [:editing]))
              (handle-editing-event application-state editor event)
              application-state)))


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

          (assoc :mouse-entered-handler (fn [application-state]
                                          (dataflow/define-to application-state (concat editor-path [:mouse-over]) true))

                 :mouse-left-handler (fn [application-state]
                                       (dataflow/define-to application-state (concat editor-path [:mouse-over]) false)))))))

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

                                                                                  (fn [application-state new-value]
                                                                                    (dataflow/define-to
                                                                                      application-state
                                                                                      (concat item-list-view-path [:items item-id])
                                                                                      new-value)))

                                                                          (view/add-mouse-event-handler (fn [application-state event]
                                                                                                          (if (and (= (:type event)
                                                                                                                      :left-mouse-button-up)
                                                                                                                   (= (:event-handling-direction application-state) :up))
                                                                                                            (dataflow/define-to application-state (concat item-list-view-path [:selection]) index)
                                                                                                            application-state)))))))

                                              (zipper-list/items (dataflow/property item-list-view-path :item-order)))))))


(defn handle-item-list-view-event [application-state item-list-view event]
  (cond

   (input/key-pressed? event input/down)
   (dataflow/apply-to-value application-state (concat item-list-view [:selection]) inc)

   (input/key-pressed? event input/up)
   (dataflow/apply-to-value application-state (concat item-list-view [:selection]) dec)

   (input/key-pressed? event input/backspace)
   (remove-item application-state item-list-view  (get application-state (concat item-list-view [:selection])))

   :default (let [application-state (handle-editor-event application-state
                                                         (concat item-list-view (editor-id (dataflow/property-from application-state item-list-view :selected-item-id)))
                                                         event)]
              (if (not (:event-handled application-state))
                (cond
                 (input/key-pressed? event input/space)
                 (add-item application-state item-list-view  (get application-state  (concat item-list-view [:selection])) "New item" )

                 :default application-state)

                application-state))))


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

(defn handle-item-view-event [application-state item-view event]
  (let [application-state (if (= (:type event)
                                 :mouse-moved)
                            (dataflow/define-to application-state [:status] (get application-state :view-parts-under-mouse))
                            application-state)]

    (let [application-state (handle-item-list-view-event application-state (concat item-view [:item-list-view]) event)]
      (if (not (:event-handled application-state))
        (cond
         (input/key-pressed? event input/escape)
         (application/close application-state)

         :default application-state)

        application-state))))




(defn initialize [application-state]
  (let [item-list-view [:elements :item-list-view]]
    (-> application-state
        (dataflow/define-to [:status] [])
        (add-item item-list-view 0 "Foo")
        (add-item item-list-view 0 "Bar")
        (add-item item-list-view 0 "Bar2")
        (add-item item-list-view 0 "Bar3")
        (add-item item-list-view 0 "Bar4")
        (add-item item-list-view 0 "Bar5"))))

(defn start []
  (application/start 700 500
                     60
                     initialize
                     handle-item-view-event
                     item-view))


;; Mouse over test

(defn add-mouse-over [layoutable key]
  (dataflow/initialize key false)
  (view/add-mouse-event-handler layoutable
                                (fn [application-state event]
                                  (case (:type event)
                                    :mouse-entered (dataflow/define key true)
                                    :mouse-left (dataflow/define key true)
                                    nil)
                                  application-state)))

(defn mouse-over-test []
  (layout/->VerticalStack [(add-mouse-over (drawable/->Rectangle 100 100 (if (dataflow/get-value :upper)
                                                                           [1 0 0 1]
                                                                           [1 1 0 1]))
                                           (dataflow/absolute-path :upper))

                           (view/add-mouse-event-handler (drawable/->Rectangle 100 100 [1 0 1 1])
                                                         (fn [application-state event]
                                                           (case (:type event)
                                                             :mouse-entered (println "mouse-entered lower")
                                                             :mouse-left (println "mouse-left lower")
                                                             nil)
                                                           application-state))]))

(defn start-mouse-over-test []
  (application/start 700 500
                     10
                     identity
                     (fn [application-state view event] application-state)
                     mouse-over-test))

(comment
(start)
(start-mouse-over-test)

(debug/set-active-channels  ;; :view-definition
                             ;; :initialization
                             ;; :dataflow
                             ;; :events
                             ;; :view-update
                             :default)

  (debug/set-active-channels))