(def possible_event_types
  '(
    "Unknown event"
    "No event"
    "Generic out"
    "Strikeout"
    "Stolen base"
    "Defensive indifference"
    "Caught stealing"
    "Pickoff error"
    "Pickoff"
    "Wild pitch"
    "Passed ball"
    "Balk"
    "Other advance"
    "Foul error"
    "Walk"
    "Intentional walk"
    "Hit by pitch"
    "Interference"
    "Error"
    "Fielder's choice"
    "Single"
    "Double"
    "Triple"
    "Home run"
    "Missing play"))

(def path "/Users/ssorrell/Google Drive/silverballs/2013_events/")

(defn id [_type possible_types]
  (.indexOf  possible_types _type))

(id "Strikeout" possible_event_types)


(defn parsed-csv [csv]
  (let [raw (slurp csv)]
    (map #(clojure.string/split (clojure.string/replace % "\"" "")
                                #"\,")
         (clojure.string/split-lines raw))))

(def headers
  (clojure.string/split (slurp (str path "default_headers.txt"))
                        #"\n"))

headers

;(id "inning" headers)

(def db
  (parsed-csv (str path "2013_EventFile.txt")))

(def event_types
  (map #(nth % (id "batter event flag" headers))
       db))

; strikeouts
(def k (map
        #(str (id % possible_event_types))
        '("Strikeout")))

(defn include [l needle]
  (some #(= needle %)
        l))

(defn compact [l]
  (filter #(identity %) l))

;(nths '(1 3 4) '(1 2 3 4 5))
(defn nths [indicies l]
  (map #(nth l %)
       indicies))

(defn db-by [k v db]
  (compact (filter #(= (nth % (id k headers))
                       v)
                   db)))

(defn game [g db]
  (db-by "game id" g db))

(defn player [p_id db]
  (db-by "res batter" p_id db))

(defn strikeouts [db]
  (count (filter #(include k %)
                 (map #(nth % (id "event type" headers))
                      db))))



(= (strikeouts (player "cabrm001" (game "MIN201304010" db)))
   2)

