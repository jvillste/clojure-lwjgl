(ns clojure-lwjgl.test.scanner
  (:require (clojure-lwjgl [window :as window]
                           [font :as font]
                           [vector-rectangle :as vector-rectangle]
                           [input :as input])
            (clojure-lwjgl.command [text :as text]
                                   [image :as image]
                                   [command :as command]
                                   [translate :as translate]
                                   [scale :as scale]
                                   [push-modelview :as push-modelview]
                                   [pop-modelview :as pop-modelview])
            (clojure.java [shell :as shell]
                          [io :as io]))
  (:import [org.lwjgl.opengl GL11 GL20 ARBVertexBufferObject ARBVertexProgram ARBVertexShader]
           [java.awt Color Font FontMetrics RenderingHints]
           [clojure_lwjgl.triangle_batch TriangleBatch]
           [java.io File]))


(defn file-name-prefix [{:keys archive-path document-number page-number}]
  (str archive-path "/" document-number "_" page-number))

(defn files-in-directory [directory-path]
  (reduce (fn [files file] (if (.isDirectory file)
                             (concat files (files-in-directory (.getPath file)))
                             (conj files file)))
          []
          (.listFiles (File. directory-path))))

(defn scan [status]
  (reset! status "Scanning...")
  (with-open [output-stream (io/output-stream "scanned.tif")]
    (io/copy (:out (shell/sh "scanimage" "--resolution" "300" "--format=tiff" "-x" "210" "-y" "297"  :out-enc :bytes))
             output-stream))
  (reset! status "Scanning ready."))

(defn create-preview [status file-name-prefix]
  (reset! status "Creating preview...")
  (shell/sh "convert" "--resolution" "'500x500'" "scanned.tif" (str file-name-prefix "_preview.jpg"))
  (reset! status "Preview ready."))

(defn start-scanning [application file-name-prefix]
  (.start (Thread. (fn []
                     (scan (:status application))
                     (create-preview (:status application)
                                     (file-name-prefix application))
                     (reset! (:preview-changed application) true)))))

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
  (assoc-in application [:view-part-command-runners view-part] (command/command-runners-for-commands ((get-in application [:view-parts view-part]) application))))

(defn update-view [application]
  (reduce update-view-part application (:view application)))

(defrecord ViewPart [dependencies function])

(defn background [{:keys [width height]}]
  [(vector-rectangle/rectangle 0 0
                               width height
                               [1 1 1 1])])

(defn foreground [{:keys [width height document-number status]}]
  (let [font (font/create "LiberationSans-Regular.ttf" 20)]
    [(text/create 0
                  0
                  (str "Document number " document-number)
                  font
                  [0.0 0.0 0.0 1.0])
     (text/create 0
                  (font/height font)
                  (str "Status: " @status)
                  font
                  [0.0 0.0 0.0 1.0])]))

(defn preview [{:keys [width]}]
  (if (.exists (File. "preview.jpg"))

    [(push-modelview/->PushModelview)
     (translate/->Translate (- width 400) 30)
     (scale/->Scale 3)
     (image/create 0
                   0
                   "out.jpg")
     (pop-modelview/->PopModelview)]
    
    []))

(defn key-pressed [keyboard-event key]
  (and (= (:key-code keyboard-event)
          key)
       (= (:type keyboard-event)
          :key-pressed)))

(defn handle-event [application event]
  (cond

   (key-pressed event input/down)
   (update-in application [:document-number] dec)

   (key-pressed event input/up)
   (update-in application [:document-number] inc)

   (key-pressed event input/space)
   (do (start-scanning application)
       application)

   :default application))

(defn handle-events [application]
  (let [unread-events (concat (input/unread-keyboard-events)
                              (input/unread-mouse-events))]
    (if (empty? unread-events)
      application
      (-> (reduce handle-event application unread-events)
          render))))

(defn update-preview [application]
  (if @(:preview-changed application)
    (update-view-part application :preview)
    application))

(defn update [application]
  (-> application
      (handle-events)
      (update-view-part :foreground)
      (update-preview)
      (render)))

(defn create-application [window]
  (-> {:archive-path "/home/jukka/Pictures/scan"
       :view-parts {:background background
                    :foreground foreground
                    :preview preview}
       :view [:background
              :preview
              :foreground]
       :preview-changed (atom false)
       :status (atom "Started")
       :view-part-command-runners {}
       :width @(:width window)
       :height @(:height window)
       :document-number 0
       :page-number 0}
      (update-view)))

(defn start []
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
                      (update-view)))))

(comment
  (start)


  (defrecord CommandRunnerBatch [command-runners]
    CommandRunner
    (delete [command-runner-batch] (update-in command-runner-batch [:command-runners] #(doall (map delete %))))
    (run [command-runner-batch] (update-in command-runner-batch [:command-runners] #(doall (map run %))))))