(ns flow-gl.threading)

(defmacro when-> [argument condition body]
  `(if ~condition
     (-> ~argument ~body)
     ~argument))
