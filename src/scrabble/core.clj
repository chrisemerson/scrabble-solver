(ns scrabble.core
    (:gen-class))

; How many ways, from the 100 standard scrabble tiles, can you choose seven which total 46 points?

(def scores
    {\A 1, \B 3, \C 3, \D 2, \E 1, \F 4, \G 2, \H 4,  \I 1,
     \J 8, \K 5, \L 1, \M 3, \N 1, \O 1, \P 3, \Q 10, \R 1,
     \S 1, \T 1, \U 1, \V 4, \W 4, \X 8, \Y 4, \Z 10, \_ 0})

(def distribution
    {\A 9, \B 2, \C 2, \D 4, \E 12, \F 2, \G 3, \H 2, \I 9,
     \J 1, \K 1, \L 4, \M 2, \N 6,  \O 8, \P 2, \Q 1, \R 6,
     \S 4, \T 6, \U 4, \V 2, \W 2,  \X 1, \Y 2, \Z 1, \_ 2})

(defn take-tile
    "Take the given tile from the bag and return the resulting bag"
    [bag tile]
    ; Filter returns a list rather than a map so we have to coerce everything back into a map
    (into {} (filter
        ; Filter out any tiles that have 0 count left
        #(pos? (second %))
        ; update the bag map, using the 'dec' function on the value associated with the key 'tile'
        (update bag tile dec))))

(defn score-hand
    "Score a hand using the scores above"
    [hand]
    ; Map each tile to its score then sum the resulting vector with reduce
    (reduce + (map scores hand)))

(defn possible-hands
    "Return a list of possible hands after taking _count_ tiles from _bag_ and adding them to _current-hand_."
    [current-hand bag count]
    (if (= count 0)
        ; If our hand is full, just return the current hand in a list on its own
        (list current-hand)
        ; Filter out duplicate hands by sorting each one and taking distinct hands from the result
        (distinct (map sort
            ; Flatten results into a single list of hands, where a hand is a vector [] of chars representing tiles
            (apply concat (map
                ; Recurse function - for each possible current hand, try adding each tile left in the bag to it.
                ; Repeat the process with the new list until our hand is full.
                #(possible-hands (conj current-hand %) (take-tile bag %) (dec count))
                (keys bag)))))))

(defn possible-hands-of-given-size-and-score
    "Returns all possible hands given a starting hand that are a certain size and score"
    [size score initial-hand]
    ; Filter all possible hands for those that match the required score
    (filter
        #(= (score-hand %) score)
        ; Get all possible hands given the initial hand and a given size.
        (possible-hands
            initial-hand
            ; Take the tiles that we have in our initial hand out of the bag before starting
            (reduce take-tile distribution initial-hand)
            size)))

(defn -main
    [& args]
    ; We must have the Z, Q, X and J tiles in our hand in order to reach a score of 46.
    ; Without all of these the maximum we can get is 45.
    (let [hands (possible-hands-of-given-size-and-score 3 46 [\Z \Q \X \J])]
        (run! println hands)
        (println)
        (print "Total count: ")
        (println (count hands))))
