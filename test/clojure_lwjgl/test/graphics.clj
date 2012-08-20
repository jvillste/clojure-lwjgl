(ns clojure-lwjgl.test.graphics
  (:require (clojure-lwjgl [window :as window]
                           [font :as font]
                           [vector-rectangle :as vector-rectangle]
                           [triangle-list :as triangle-list]
                           [logged-access :as logged-access])
            (clojure-lwjgl.command [text :as text]
                                   [command :as command]))
  (:import [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader]
           [java.awt Color Font FontMetrics RenderingHints]
           [clojure_lwjgl.triangle_batch TriangleBatch]
           [clojure_lwjgl.triangle_list TriangleList]))


(defn draw-view-part [application view-part]
  (doseq [command-runner (get-in application [:view-part-command-runners view-part])]
    (command/run command-runner)))

(defn render [application]
  (GL11/glClearColor 0 0 0 0)
  (GL11/glClear GL11/GL_COLOR_BUFFER_BIT)

  (doseq [view-part (:view application)]
    (draw-view-part application view-part))

  application)

(defn update-view-part [application view-part]
  (doseq [command-runner (get-in application [:view-part-command-runners view-part])]
    (command/delete command-runner))
  (logged-access/with-access-logging
    (assoc-in application [:view-part-command-runners view-part] (command/command-runners-for-commands ((:function (get-in application [:view-parts view-part])) application)))))

(defn update-view [application]
  (reduce update-view-part application (:view application)))

(defrecord ViewPart [dependencies function])

(defn background [application]
  (let [width (logged-access/logged-get application :width)
        height (logged-access/logged-get application :height)]
    [(vector-rectangle/rectangle 20 20
                                 (- width 40)
                                 (- height 40)
                                 [0.5 0.5 0.5 1])
     (text/create (/ width 2)
                  (- (/ height 2)
                     100)
                  "JEES"
                  (font/create "LiberationSans-Regular.ttf" 40)
                  [0.0 0.0 0.0 1.0])
     (vector-rectangle/rectangle 30 30
                                 (- width 60)
                                 (- height 60)
                                 [1 1 1 0.5])]))

(defn foreground [application]
  (let [width (logged-access/logged-get application :width)
        height (logged-access/logged-get application :height)
        count (logged-access/logged-get application :count)]
    [(text/create (/ width 2)
                  (/ height 2)
                  (str count)
                  (font/create "LiberationSans-Regular.ttf" 40)
                  [0.0 0.0 0.0 1.0])]))

(defn update [application]
  (-> application
      (update-in [:count] inc)
      (update-view-part :foreground)
      (render)))

(defn create-view-part [view-part-function]
  (logged-access/with-access-logging
    (->ViewPart #{} view-part-function)))

(defn create-application [window]
  (-> {:view-parts {:background (create-view-part background)
                    :foreground (create-view-part foreground)}
       :view [:background
              :foreground]
       :view-part-command-runners {}
       :width @(:width window)
       :height @(:height window)
       :count 0}
      (update-view)))


(comment

(window/start 700 500
                2
                create-application
                update
                identity
                (fn [state width height]
                  (-> state
                      (assoc
                          :width width
                          :height height)
                      (update-view))))

  (defrecord CommandRunnerBatch [command-runners]
    CommandRunner
    (delete [command-runner-batch] (update-in command-runner-batch [:command-runners] #(doall (map delete %))))
    (run [command-runner-batch] (update-in command-runner-batch [:command-runners] #(doall (map run %))))))