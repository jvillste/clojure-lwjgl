(ns flow-gl.gui.drawable
  (:require  (flow-gl.graphics.command [text :as text])
             (flow-gl.graphics [font :as font]
                               [vector :as vector])
             (flow-gl.gui [layoutable :as layoutable])))

(defprotocol Drawable
  (drawing-commands [drawable]))

;; DRAWABLES

(defrecord Text [contents font color]
  Drawable
  (drawing-commands [text]
    [(text/create 0 0
                  contents
                  font
                  color)])

  layoutable/Layoutable
  (preferred-width [text] (font/width font contents))
  (preferred-height [text] (font/height font))

  Object
  (toString [this] (layoutable/describe-layoutable this "Text" :contents :font :color)))

(defrecord Empty []
  Drawable
  (drawing-commands [empty] [])

  layoutable/Layoutable
  (preferred-width [empty] 0)
  (preferred-height [empty] 0)

  Object
  (toString [this] (layoutable/describe-layoutable this "Empty")))

(defrecord Rectangle [width height color]
  Drawable
  (drawing-commands [rectangle]
    [(vector/rectangle 0
                       0
                       width
                       height
                       color)])
  layoutable/Layoutable
  (preferred-width [rectangle] width)
  (preferred-height [rectangle] height)

  Object
  (toString [this] (layoutable/describe-layoutable this "Rectangle" :color)))

(defrecord Triangle [color x1 y1 x2 y2 x3 y3]
  Drawable
  (drawing-commands [rectangle]
    (vector/triangle color x1 y1 x2 y2 x3 y3))

  layoutable/Layoutable
  (preferred-width [rectangle] (max x1 x2 x3))
  (preferred-height [rectangle] (max y1 y2 y3))

  Object
  (toString [this] (layoutable/describe-layoutable this "Triangle" :color :x1 :y1 :x2 :y2 :x3 :y3)))
