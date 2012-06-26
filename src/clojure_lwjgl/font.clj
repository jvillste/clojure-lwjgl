(ns clojure-lwjgl.font
  (:import [org.lwjgl.opengl GL11]
           [org.lwjgl BufferUtils]
           [java.io File]
           [java.awt.image BufferedImage]
           [java.awt Color Font FontMetrics RenderingHints]))

(defn create [ttf-file-name size]
  (let [font (-> (Font/createFont Font/TRUETYPE_FONT (File. ttf-file-name))
                 (.deriveFont (float size)))]
    {:font font
     :font-metrics (let [graphics (.getGraphics (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB))]
                     (.setFont graphics font)
                     (.getFontMetrics graphics))}))

(defn width [font text] (.stringWidth (:font-metrics font) text))

(defn height [font text] (+ (.getMaxAscent (:font-metrics font))
                            ;;    (.getLeading (:font-metrics font))
                            (.getMaxDescent (:font-metrics font))))

(defn ascent [font] (.getMaxAscent (:font-metrics font)))

(defn graphics-font [font] (:font font))


