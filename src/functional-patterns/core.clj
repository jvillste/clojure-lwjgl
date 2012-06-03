(ns functional-patterns.core)

(def *state* (atom {}))

(defn create [] {:foo 1})

(defn update [state]
  (update-in state [:foo] #(+ % 1)))

(defn apply-update [state]
  (let [new-state (update state)]
    (swap! *state* new-state)
    new-state))