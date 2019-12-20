(ns advent2019.day-15
  (:require [advent2019.lib.intcode :as intcode]
            [loom.graph :as g]
            [loom.label :as gl]
            [loom.alg :as alg]))

(defn command->input [command]
  (case command
    :north 1
    :south 2
    :west 3
    :east 4))

(defn output->result [output]
  (case output
    0 :wall
    1 :moved
    2 :found))

(defn move [position command result]
  (let [position' (case command
                    :north (update position :y inc)
                    :south (update position :y dec)
                    :west (update position :x dec)
                    :east (update position :x inc))]
    (case result
      :wall position
      :moved position'
      :found position')))

(defn command [droid cmd]
  (let [continuation (:continuation droid)
        continuation' (intcode/resume-program-with-input continuation (command->input cmd))
        result (output->result (:output continuation'))]
    {:result result
     :droid {:continuation (intcode/resume-program continuation')
             :position (move (:position droid) cmd result)}}))

; recursively explore in all directions and hope we don't blow the stack
(defn explore [droid graph]
  (let [position (:position droid)
        directions [:north :south :west :east]]
    (reduce (fn [acc dir-graph] (g/graph acc dir-graph))
            graph
            (map (fn [direction]
                   (let [result (command droid direction)
                         droid (:droid result)
                         position' (:position droid)]
                     (if (g/has-node? graph position')
                       graph
                       (case (:result result)
                         :wall graph
                         :moved (explore droid
                                         (-> graph
                                             (g/add-nodes position')
                                             (g/add-edges [position position'])))
                         :found (explore droid
                                         (-> graph
                                             (gl/add-labeled-nodes position' "destination")
                                             (g/add-edges [position position'])))))))
                 directions))))

(defn solve! [file]
  (let [start {:x 0 :y 0}
        droid {:continuation (intcode/run-program (intcode/load-program! file))
               :position start}
        graph (explore droid (g/add-nodes (g/graph)
                                          start))
        destination (->> (g/nodes graph)
                         (map #(assoc {:node %} :label (gl/label graph %)))
                         (filter #(not (nil? (:label %))))
                         (first)
                         (:node))]
    (println "Fewest number of movement commands:"
             (dec (count (alg/bf-path graph start destination))))))
