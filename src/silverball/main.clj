(use 'clojure.repl)

(ns siverball.main)

(print *ns*)

(def root_dir (-> (-> *file* clojure.java.io/file .getParent
                      clojure.java.io/file .getParent
                      clojure.java.io/file .getParent)))

(def path (str root_dir "/res/2013_eventdata/"))

(def category_map {
  :1B	true
  :2B true
  :3B true
  :BB true
  :HR true
  :HP true
  :R false
  :RBI false
  :SB false
  :KO false
  :GDP false
  :CS false
  :SAC false

  :BBI false
  :HA	false
  :HB false
  :ER false
  :IP false
  :K  false
  :L false
  :S false
  :W false
  :CG false
  :BS false
})

(defn are_we_there_yet [category]
    (category category_map)
  )

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

;ruby envy. please to stdlib
(defn compact [l]
  (filter #(identity %) l))

(defn detect [pred l]
  (first (filter pred l)))

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
  (count-by (event-type "Walk")
            db))

(walks (game "MIN201304010" db))

(defn singles [db]
  (count-by (event-type "Single")
            db))

(singles (game "MIN201304010" db))

(defn doubles [db]
  (count-by (event-type "Double")
            db))

(doubles (game "MIN201304010" db))

(defn triples [db]
  (count-by (event-type "Triple")
            db))

(triples (game "NYA201304010" db))

(defn home_runs [db]
  (count-by (event-type "Home run")
            db))

(home_runs (game "NYA201304030" db))

(defn hit_by_pitches [db]
  (count-by (event-type "Hit by pitch")
            db))

(hit_by_pitches (game "NYA201304030" db))
;;
(def all_possible_event_texts
  (map #(nth % (id "event text" headers))
       db))

(defn who_was_on [base event]
  (let [event_text (nth event (id "event text" headers))]
    (case base
      "1" (nth event (id "first runner" headers))
      "2" (nth event (id "second runner" headers))
      "3" (nth event (id "third runner" headers))
      "H" (nth event (id "res batter" headers)))))

(defn batter_result [event_text]
  (let [hit_type (last (re-find #"([A-Z]*).*/" event_text))]
    (case hit_type
      "HR" "H"
      "S" "1"
      "W" "1"
      "D" "2"
      "T" "3"
      nil
      )


    ))

(batter_result "HR/L.1-H;2-H")

(re-find #"([A-Z]*).*/"
         "D9/L")

(= "2"
   (batter_result
    "D9/L"))

(batter_result "46/L.1-H")

(conj '("b") "a")

(defn who_is_on [base event]
  (let [event_text (nth event (id "event text" headers))
        br_pos (base_runner_positions event_text)
        from_bases (map first (filter #(= (last %)
                                          base)
                                      br_pos))
        batter_result (batter_result event_text)]
    (map #(who_was_on %
                      event)
         from_bases)))

(who_is_on "3" _event)

(nth _event (id "first runner" headers))

()

(def _grand_slam_event
  ["ARI201304280" "COL" "2" "1" "1" "2" "2" "0" "0" "chave001" "L" "garlj001" "R" "montm001" "cabrm001" "rizza001" "HR/L.1-H;2-H;3-H" "F" "F" "5" "6" "20" "T" "T" "1" "F" "F" "0" "0" "F" "F" "0" "1" "3" "0" "0"]

  )

(def _event
  ["ARI201304280" "COL" "2" "1" "1" "2" "2" "0" "0" "chave001" "L" "garlj001" "R" "montm001" "" "" "S9/L.1-3" "F" "F" "5" "6" "20" "T" "T" "1" "F" "F" "0" "0" "F" "F" "0" "1" "3" "0" "0"]
  )


(clojure.test/is (=
                  (who_is_on "3"
                             _event)
                  '("montm001")))

; todo write testing macro to place units near to funcs but print hole file green/red
(clojure.test/is (=
                  (who_was_on "1"
                              _event)
                  "montm001"))

(defn scoring_players [event]
  (who_is_on "H" event))

(scoring_players _grand_slam_event)

(defn maybe-list [x l]
  (if (nil? l) nil
    (list x l)))

(defn maybe-cons [x l]
  (if (nil? x) l
    (cons x l)))


;returns a seq of batter movements in tuples (including batters. batter outcomes are not included for those batters who do not reach base)
; this method is soup, but it wraps up batter movement adequetly
(defn base_runner_positions [event_text]
  (let [base_runners (compact (map #(clojure.string/split (last %) #"-")
                                   (re-seq #"([1|2|3|H]-[1|2|3|H])"
                                           event_text)))
        batter (maybe-list "H" (batter_result event_text))]
    (compact (maybe-cons batter base_runners))))

(= '(("H" "1")
     ("2" "H"))
   (base_runner_positions "S7/G.2-H"))

(base_runner_positions "46/2-H")

;unit test BRA
(def test_event "S8/G.2-H;H-1;1-3")

(=  (base_runner_positions test_event)
    '(["2" "H"] ["H" "1"] ["1" "3"]))

;integration testing, do you do it?

(= (strikeouts (hitter "cabrm001"
                       (game "MIN201304010" db)))
   2)

(=
 (caught_stealing (game "ARI201304010" db))
 '("parrg001"))

(=
 (walks (game "ARI201304010" db))
 1)


(=
 (stolen_base
  (game "PIT201304040" db))
 '("mccua001", "mccua001", "rizza001"))
