(def root_dir (->  *file* clojure.java.io/file .getParent))

(def path (str root_dir "/2013_eventdata/"))

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

(defn id [_type possible_types]
  (.indexOf  possible_types _type))

(defn parsed-csv [csv]
  (let [raw (slurp csv)]
    (map #(clojure.string/split (clojure.string/replace % "\"" "")
                                #"\,")
         (clojure.string/split-lines raw))))

(def headers
  (clojure.string/split (slurp (str path "default_headers.txt"))
                        #"\n"))

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
  (boolean
   (some #(= needle %) l)))

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

(defn hitter [p_id db]
  (db-by "res batter" p_id db))

(defn event-type [type]
  (map str (list (id type possible_event_types))))

(defn strikeouts [db]
  (count-by (event-type "Strikeout")
            db))

(defn caught_stealing [db]
  (map #(who-was-caught-stealing %)
       (rows-filtered-by (event-type "Caught stealing") db)))

(defn count-by [event_type_keys db]
  (count (filter-by event_type_keys db)))

(defn rows-filtered-by [event_type_keys db]
  (filter #(include event_type_keys
                    (nth % (id "event type" headers)))
          db))

(defn filter-by [event_type_keys db]
  (filter #(include event_type_keys %)
          (map #(nth % (id "event type" headers))
               db)))

(defn row-with-event-type [e_type]
  (filter #(include e_type (nth % (id "event type" headers)))
          db))

(defn who-stole-a-base [sb_event]
  (let [event_s (nth sb_event (id "event text" headers))
        the_stolen_base
        (second (re-find (re-pattern #"SB(\d+)")
                         event_s))]
    (case the_stolen_base
      "2" (nth sb_event (id "first runner" headers))
      "3" (nth sb_event (id "second runner" headers))
      "H" (nth sb_event (id "third runner" headers))
      nil)))

(defn who-was-caught-stealing [cs_event]
  (let [event_s (nth cs_event (id "event text" headers))
        caught_stealing_at
        (second (re-find (re-pattern #"CS(\d+)")
                         event_s))]
    (case caught_stealing_at
      "2" (nth cs_event (id "first runner" headers))
      "3" (nth cs_event (id "second runner" headers))
      "H" (nth cs_event (id "third runner" headers))
      nil)))

(defn stolen_base [db]
  (map #(who-stole-a-base %)
       (rows-filtered-by (event-type "Stolen base") db)))

(defn walks [db]
  (count-by(event-type "Walk")
           db))
;integration testing, do you do it?

(= (strikeouts (hitter "cabrm001" (game "MIN201304010" db)))
   2)

(=
 (caught_stealing (game "ARI201304010" db))
 '("parrg001"))

(=
 (walks (game "ARI201304010" db))
 1)

(rows-filtered-by (event-type "Stolen base")
                  db)

(=
 (stolen_base
  (game "PIT201304040" db))
 '("mccua001", "mccua001", "rizza001"))
