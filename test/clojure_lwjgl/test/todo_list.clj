(ns clojure-lwjgl.test.todo-list
  (:require (clojure-lwjgl [window :as window]
                           [font :as font]
                           [vector-rectangle :as vector-rectangle]
                           [input :as input]
                           [logged-access :as logged-access])
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
            (clojure [string :as string]))
  (:import [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader]
           [java.awt Color Font FontMetrics RenderingHints]
           [clojure_lwjgl.triangle_batch TriangleBatch]
           [java.io File]))

(defrecord ViewPartCall [id function])
(extend ViewPartCall
  command/Command
  {:create-runner identity}
  command/CommandRunner
  {:delete identity
   :run identity})

(defn draw-view-part [application view-part]
  (println "drawing " view-part )
  (doseq [command-runner (get-in application [:view-part-command-runners view-part])]
    (if (instance? ViewPartCall command-runner)
      (draw-view-part application (:id command-runner))
      (command/run command-runner))))

(defn render [application]
  (println "render")
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)

  (dorun (map (partial draw-view-part application)
              (:view application)))

  application)


(defn schedule-thread [application function]
  (update-in application [:scheduled-threads] conj function))

(defn launch-scheduled-threads [application]
  (doseq [function (:scheduled-threads @application)]
    (.start (Thread. (fn [] (function application)))))
  (swap! application #(assoc % :scheduled-threads #{})))

(defn key-pressed [keyboard-event key]
  (and (= (:key-code keyboard-event)
          key)
       (= (:type keyboard-event)
          :key-pressed)))

(defn handle-event [application event]
  (cond

   (key-pressed event input/down)
   (dataflow/apply-to-value application :selection inc)

   (key-pressed event input/up)
   (dataflow/apply-to-value application :selection dec)

   :default application))

(defn handle-events [application]
  (let [unread-events (concat (input/unread-keyboard-events)
                              (input/unread-mouse-events))]
    (if (empty? unread-events)
      application
      (reduce handle-event application unread-events))))

(defn define-view-part-calls [application-state view-part-calls]
  (reduce (fn [application-state view-part-call]
            (-> (dataflow/define application-state
                  [:view-parts (:id view-part-call)] (:function view-part-call))
                (assoc :defined-view-part-calls true)))
          application-state
          view-part-calls))

(defn update-view [application]
  (when (dataflow/changes @application)
    (let [application-state (swap! application #(-> %
                                                    (assoc :changes-to-be-processed (dataflow/changes %))
                                                    (dataflow/reset-changes)))

          changes (:changes-to-be-processed application-state)

          changed-view-parts (->> (filter #(= (first %)
                                              :view-parts)
                                          changes)
                                  (map second))

          undefined-view-part-calls (->> changed-view-parts
                                         (mapcat #(get-in application-state [:view-parts %]))
                                         (filter #(and (instance? ViewPartCall %)
                                                       (not (contains? application-state
                                                                       [:viewparts (:id %)])))))

          new-command-runners (reduce (fn [command-runners view-part]
                                        (println "updating " view-part)
                                        (dorun (map command/delete (get command-runners view-part)))
                                        (assoc command-runners
                                          view-part (command/command-runners-for-commands (get-in application-state
                                                                                                  [:view-parts view-part]))))
                                      (:view-part-command-runners application-state)
                                      changed-view-parts)]

      (let [application-state (swap! application
                                     #(-> %
                                          (assoc :view-part-command-runners new-command-runners)
                                          (define-view-part-calls undefined-view-part-calls)))]
        (println "foo")
        (if (empty? undefined-view-part-calls)
          (render application-state)
          (update-view application))))))

(defn update [application]
  (println "update")
  (swap! application handle-events)
  (launch-scheduled-threads application)
  (update-view application)

  application)

(comment
  (view
   (view-part)))


(defn editor [item-index selected]
  #(do (println "running editor " item-index (dataflow/get-value-in [:items item-index]))
       (vector (vector-rectangle/rectangle 0 0
                                           100 30
                                           (if selected
                                             [0 0 1 1]
                                             [0.9 0.9 1 1]))
               (text/create 5 5 (dataflow/get-value-in [:items item-index]) (font/create "LiberationSans-Regular.ttf" 15) [0.0 0.0 0.0 1.0]))))

(defmacro call-view-part [view-part-creator & arguments]
  `(->ViewPartCall [~view-part-creator ~@arguments]
                   (~view-part-creator ~@arguments)))

(def view
  [#(dataflow/with-values [width height]
      [(vector-rectangle/rectangle 0 0
                                   width height
                                   [1 1 1 1])])

   #(dataflow/with-values [item-order selection]
      (flatten (map-indexed (fn [line-number item-index]
                              [(push-modelview/->PushModelview)
                               (translate/->Translate 0
                                                      (* line-number 30))
                               (call-view-part editor
                                               item-index
                                               (= selection
                                                  line-number))
                               (pop-modelview/->PopModelview)])
                            item-order)))])



(defn define-view [application view-parts]
  (reduce (fn [application [index view-part]]
            (-> application
                (dataflow/define [:view-parts (keyword (str index))] view-part)
                (update-in [:view] conj (keyword (str index)))))
          (assoc application :view [])
          (map-indexed vector view-parts)))

(defn create-application [window]
  (println "Creating application")
  (-> (dataflow/create)
      (dataflow/define
        :width  @(:width window)
        :height  @(:height window)
        :selection 0
        [:items 1] "Foo"
        [:items 2] "bar"
        :item-order [1 2]
        :view-part-command-runners {}
        :scheduled-threads #{})
      (define-view view)
      (atom)))

(defn start []
  (window/start 700 500
                60
                create-application
                update
                identity
                (fn [application width height]
                  (println "resize callback")
                  (swap! application
                         #(dataflow/define
                            %
                            :width width
                            :height height))
                  application)))

(comment
  (start))