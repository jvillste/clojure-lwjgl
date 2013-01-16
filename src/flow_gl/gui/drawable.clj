(ns flow-gl.gui.drawable
  (:require  (flow-gl.graphics.command [text :as text])
             (flow-gl.graphics [font :as font]
                               [vector :as vector])
             (flow-gl.gui [layout :as layout])))

(defprotocol Drawable
  (drawing-commands [element width height]))

;; DRAWABLES

(defrecord Text [contents font color]
  Drawable
  (drawing-commands [text width height]
    [(text/create 0 0
                  contents
                  font
                  color)])

  layout/Layoutable
  (preferred-width [text] (font/width font contents))
  (preferred-height [text] (font/height font))

  Object
  (toString [this] (layout/describe-layoutable this "Text" :contents :font :color)))

(defrecord Rectangle [width height color]
  Drawable
  (drawing-commands [rectangle requested-width requested-height]
    [(vector/rectangle 0
                       0
                       requested-width
                       requested-height
                       color)])
  layout/Layoutable
  (preferred-width [rectangle] width)
  (preferred-height [rectangle] height)

  Object
  (toString [this] (layout/describe-layoutable this "Rectangle" :color)))
