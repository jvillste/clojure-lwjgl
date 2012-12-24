(ns flow-gl.graphics.command)

(defprotocol Command
  (create-runner [command]))

(defprotocol CombinableCommand
  (combine [command other-command]))

(defprotocol CommandRunner
  (delete [command-runner])
  (run [command-runner]))

(defn command-runners-for-commands [commands]
  (flatten (map (fn [command-group]
                  (if (satisfies? CombinableCommand (first command-group))
                    (-> (reduce combine command-group)
                        (create-runner))
                    (map create-runner command-group)))
                (partition-by type commands))))