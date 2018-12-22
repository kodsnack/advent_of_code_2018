(ns adventofcode.2018.day08
  (:require clojure.string))

(defn parse-input [lines]
  (read-string (str "(" (first lines) ")")))

(defn parse-node [children meta num-children num-meta rest]
  (cond
    (> num-children 0)
    (let [[nc nm & rest] rest
          [child rest] (parse-node [] [] nc nm rest)]
      (recur (conj children child) meta (dec num-children) num-meta rest))

    (> num-meta 0)
    (let [[next-meta & rest] rest]
      (recur children (conj meta next-meta) num-children (dec num-meta) rest))

    :else
    [[children meta] rest]
    ))

(defn parse-tree [[num-children num-meta & rest]]
  (first (parse-node [] [] num-children num-meta rest)))

(defn get-children [node] (first node))
(defn get-meta [node] (second node))

(defn all-meta [meta nodes]
  (if (seq nodes)
    (recur (apply conj meta (mapcat get-meta nodes)) (mapcat get-children nodes))
    meta
    ))

(defn node-value [node]
  (if (empty? (get-children node))
    (apply + (get-meta node))
    (->> node
         (get-meta)
         (keep (fn [child-i]
                (get (get-children node) (dec child-i))
                ))
         (map node-value)
         (apply +)
         )))

(defn solve-a [lines]
  (->> lines
       (parse-input)
       (parse-tree)
       (vector)
       (all-meta [])
       (apply +)
       ))

(defn solve-b [lines]
  (->> lines
       (parse-input)
       (parse-tree)
       (node-value)
       ))

(defn run [input-lines & args]
  {:A (solve-a input-lines)
   :B (solve-b input-lines)
   }
  )
