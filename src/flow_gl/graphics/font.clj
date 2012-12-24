(ns flow-gl.graphics.font
  (:import [java.io File]
           [java.awt.image BufferedImage]
           [java.awt Color Font FontMetrics RenderingHints]))

(def loaded-fonts (atom {}))

(defn create [ttf-file-name size]
  (swap! loaded-fonts
         (fn [loaded-fonts]
           (if (contains? loaded-fonts [ttf-file-name size])
             loaded-fonts
             (assoc loaded-fonts
               [ttf-file-name size]
               (let [font (-> (Font/createFont Font/TRUETYPE_FONT (File. ttf-file-name))
                              (.deriveFont (float size)))]
                 {:font font
                  :font-metrics (let [graphics (.getGraphics (BufferedImage. 1 1 BufferedImage/TYPE_INT_ARGB))]
                                  (.setFont graphics font)
                                  (.getFontMetrics graphics))})))))
  (@loaded-fonts [ttf-file-name size]))

(defn width [font text] (.stringWidth (:font-metrics font) text))

(defn height [font] (+ (.getMaxAscent (:font-metrics font))
                       (.getMaxDescent (:font-metrics font))))

(defn ascent [font] (.getMaxAscent (:font-metrics font)))

(defn graphics-font [font] (:font font))


