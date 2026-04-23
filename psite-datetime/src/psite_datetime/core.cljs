(ns psite-datetime.core
  (:require
   [cljs-time.coerce :as time.coerce]
   [cljs-time.core :as t]
   [cljs-time.format :as time.format]))

(defn currentyear []
  (t/year (t/now)))

(defn map-vals
  "maps a function over every value in a hash-map"
  [f m]
  (into {} (for [[k v] m] [k (f v)])))

(def weekdaystrings
  {:en
   [["Monday" "Mon"]
    ["Tuesday" "Tue"]
    ["Wednesday" "Wed"]
    ["Thurdays" "Thu"]
    ["Friday" "Fri"]
    ["Saturday" "Sat"]
    ["Sunday" "Sun"]]

   :de
   [["Montag" "Mo"]
    ["Dienstag" "Di"]
    ["Mittwoch" "Mi"]
    ["Donnerstag" "Do"]
    ["Freitag" "Fr"]
    ["Samstag" "Sa"]
    ["Sonntag" "So"]]

   :fr
   [["lundi" "lun"]
    ["mardi" "mar"]
    ["mercredi" "mer"]
    ["jeudi" "jeu"]
    ["vendredi" "ven"]
    ["samedi" "sam"]
    ["dimanche" "dim"]]

   :ru
   [["Понедельник" "Пн"]
    ["Вторник" "Вт"]
    ["Среда" "Ср"]
    ["Чеверг" "Чт"]
    ["Пятница" "Пт"]
    ["Суббота" "Сб"]
    ["Воскресенье" "Вс"]]

   :uk
   [["понеділок" "Пн"]
    ["вівторок" "Вт"]
    ["середа" "Ср"]
    ["четвер" "Чт"]
    ["п’ятниця" "Пт"]
    ["субота" "Сб"]
    ["неділя" "Нд"]]})

(def monthstrings
  {:en
   [["January" "Jan"]
    ["February" "Feb"]
    ["March" "Mar"]
    ["April" "Apr"]
    ["May" "May"]
    ["June" "Jun"]
    ["July" "Jul"]
    ["August" "Aug"]
    ["September" "Sep"]
    ["October" "Oct"]
    ["November" "Nov"]
    ["December" "Dec"]]

   :de
   [["Januar" "Jan"]
    ["Februar" "Feb"]
    ["März" "März"]
    ["April" "Apr"]
    ["Mai" "Mai"]
    ["Juni" "Jun"]
    ["Juli" "Jul"]
    ["August" "Aug"]
    ["September" "Sep"]
    ["Oktober" "Okt"]
    ["November" "Nov"]
    ["Dezember" "Dez"]]

   :fr
   [["janvier" "janv"]
    ["février" "févr"]
    ["mars" "mars"]
    ["avril" "avr"]
    ["mai" "mai"]
    ["juin" "juin"]
    ["juillet" "juill"]
    ["août" "août"]
    ["septembre" "sept"]
    ["octobre" "oct"]
    ["novembre" "nov"]
    ["décembre" "déc"]]

   :ru
   [["Январь" "Янв"]
    ["Февраль" "Фев"]
    ["Март" "Мар"]
    ["Апрель" "Апр"]
    ["Май" "Май"]
    ["Июнь" "Июн"]
    ["Июль" "Июл"]
    ["Август" "Авг"]
    ["Сентябрь" "Сен"]
    ["Октябрь" "Окт"]
    ["Ноябрь" "Ноя"]
    ["Декабрь" "Дек"]]

   :uk
   [["Січня" "Сiч"]
    ["Лютого" "Лют"]
    ["Березня" "Бер"]
    ["Квітня" "Квi"]
    ["Травня" "Трав"]
    ["Червня" "Черв"]
    ["Липня" "Лип"]
    ["Серпня" "Серп"]
    ["Вересня" "Вер"]
    ["Жовтня" "Жовт"]
    ["Листопада" "Лист"]
    ["Грудня" "Груд"]]})

(defn month->string
  ([month]
   (month->string {} month))
  ([{:keys [locale variant]
     :or   {locale  :de
            variant 0}} month-int]
   (get-in monthstrings [locale (dec month-int) variant])))

(defn- weekdaystrings-from-integer
  "weekday must be an integer between 1 and 7.
  version must be an integer (0 = long, 1 = short)."
  [weekday version]
  (map-vals
   #(-> %
        (get (- weekday 1))
        (get version))
   weekdaystrings))

(defn weekday-strings
  "Takes a timeobject and returns a map of strings corresponding to the
  weekday for every available locale. The variant (\"Sat\" vs \"Saturday\")
  can be specified by the integer VERSION, defaults to 0 (long)."
  ([timeobj version]
   (weekdaystrings-from-integer
    (t/day-of-week timeobj) version))
  ([timeobj]
   (weekday-strings timeobj 0)))

(defn monthstrings-from-integer [month-number version]
  (map-vals
   #(nth (nth % (- month-number 1)) version)
   monthstrings))

(defn month-strings
  ([timeobj version]
   (monthstrings-from-integer
    (t/month timeobj) version))
  ([timeobj]
   (month-strings timeobj 0)))

(defn format-date
  "lang can be any of :en :de :fr :ru :uk — returns a formatted datestring."
  [dateobj lang]
  (let [formatter
        (time.format/formatter
         (lang
          {:en "M/D/yyyy"
           :de "D. M. yyyy"
           :fr "D. M. yyyy"
           :ru "D. M. yyyy"
           :uk "D. M. yyyy"}))]
    (time.format/unparse formatter dateobj)))

(defn reformat-date
  "like format-date but first reads a string"
  [datestring lang]
  (format-date (time.coerce/from-string datestring) lang))

(defn format-time [timeobj lang]
  (let [formatter
        (time.format/formatter
         (lang
          {:en "h:mm a"
           :de "H:mm 'Uhr'"
           :fr "H'h'mm"
           :ru "H:mm"
           :uk "H:mm"}))]
    (time.format/unparse formatter timeobj)))

(defn format-times [timeobj]
  (map-vals
   #(time.format/unparse (time.format/formatter %) timeobj)
   {:en "h:mm a"
    :de "H:mm 'Uhr'"
    :fr "H'h'mm"
    :ru "H:mm"
    :uk "H:mm"}))

(defn unify-date-and-time [datestring timestring]
  (cond
    (and datestring timestring)
    (time.coerce/from-string (str datestring " " timestring))
    datestring
    (time.coerce/from-string datestring)
    :else
    nil))

(defn ordinalize-en [int]
  (let [intstring (str int)
        suffix
        (last
         (first
          (filter
           #(re-find (first %) intstring)
           [[#"^1[0-9]$" "th"]
            [#"1$" "st"]
            [#"2$" "nd"]
            [#"3$" "rd"]
            [#"" "th"]])))]
    (list intstring [:sup suffix])))

(defn format-dates-wordy
  "lang can be any of :en :de :fr :ru — returns a formatted datestring."
  ([dateobj type year?]
   (let [{:keys [years months days]} (time.format/instant->map dateobj)
         monthoutputs                (monthstrings-from-integer months type)
         yearstring                  (if year? (str " " years) "")]
     {:de (list days ". " (:de monthoutputs) yearstring)
      :en (list (:en monthoutputs) " " (ordinalize-en days) yearstring)
      :uk (list days ". " (:uk monthoutputs) yearstring)}))
  ([dateobj type]
   (format-dates-wordy dateobj type nil)))

(defn ptermin [dtstart duration summary supplements]
  (merge
   {:dtstart dtstart
    :dtend   (t/plus dtstart (t/minutes duration))
    :summary summary}
   supplements))

(defn- csv
  "joins vals with separator, skipping empties (mirrors putils.helpers/csv)"
  [separator & vals]
  (reduce #(str %1 separator %2)
          (remove empty? vals)))

(defn- update-multiple [m ks f]
  (reduce #(update %1 %2 f) m ks))

(defn ical-termin [ptermin]
  (let [vevent
        (apply
         csv "\n"
         (map
          #(str (.toUpperCase (name (first %))) ":" (second %))
          (update-multiple
           ptermin
           [:dtstart :dtend]
           #(str % "Z"))))]
    (csv "\n"
         "BEGIN:VCALENDAR"
         "VERSION:2.0"
         "PRODID:-//Phylax//NONSGML v1.0//EN"
         "CALSCALE:GREGORIAN"
         "METHOD:PUBLISH"
         "BEGIN:VEVENT"
         vevent
         "END:VEVENT"
         "END:VCALENDAR")))

(def google-cal-converters
  "maps ptermin-keywords (iCal format) to their google calendar counterparts."
  {:text     :summary
   :location :location
   :details  :description
   :dates    #(str (:dtstart %) "/" (:dtend %))
   :sprop    #(str "website:" (:url %))})

(defn google-calendar [ptermin]
  (reduce str
          "https://www.google.com/calendar/event?action=TEMPLATE"
          (map #(str "&" (name (first %)) "=" (js/encodeURI ((second %) ptermin)))
               google-cal-converters)))

(defn add-days-to-string [days datestring]
  (time.format/unparse
   (time.format/formatter "yyyy-MM-dd")
   (t/plus
    (time.coerce/to-date-time datestring)
    (t/days days))))

(defn datestring-arith [datestring unit amount operation]
  (time.format/unparse
   (time.format/formatter "yyyy-MM-dd")
   (operation
    (time.coerce/to-date-time datestring)
    (unit amount))))

(defn individual-days [first-day last-day]
  (loop [day       (time.coerce/to-date-time first-day)
         interdays []]
    (if (> day (time.coerce/to-date-time last-day))
      interdays
      (recur (t/plus day (t/days 1))
             (conj interdays day)))))

(defn count-days-str [first-day-str last-day-str]
  (/ (t/in-minutes
      (t/interval
       (time.coerce/to-date-time first-day-str)
       (time.coerce/to-date-time last-day-str)))
     1440))

(defn count-days [first-day last-day]
  (/ (t/in-minutes (t/interval first-day last-day)) 1440))

(defn to-iso-day [timeobj]
  (time.format/unparse
   (time.format/formatter "yyyy-MM-dd")
   timeobj))

(defn days-in-month [date-with-month]
  (let [first-day (t/first-day-of-the-month- date-with-month)]
    (take-while
     #(= (t/month %) (t/month first-day))
     (iterate #(t/plus- % (t/days 1)) first-day))))

(defn days-in-interval
  "returns individual days, as time-instances 00:00 contained in an interval.
  t/floor keeps the time component (20200913000000 vs 20200913) so t/= works."
  [start end]
  (take-while
   #(t/before? % end)
   (iterate
    #(-> %
         (t/plus- (t/days 1))
         (t/floor t/day))
    start)))

(defn within-left?
  "checks like time.core/within? but also allows the date to coincide with the start"
  [start end date]
  (or (t/within? start end date)
      (t/= end date)))

(defn within-inclusive? [date1 date2 testdate]
  (or (t/within? date1 date2 testdate)
      (t/= date2 testdate)))

(defn unordered-within?
  "like within? but does not require date1 to be before date2"
  ([date1 date2 testdate]
   (unordered-within? date1 date2 testdate nil))
  ([date1 date2 testdate inclusive?]
   (let [args (concat
               (sort t/before? [date1 date2])
               [testdate])]
     (apply
      (if inclusive? within-inclusive? t/within?)
      args))))

(defn datestring? [string]
  (not (js/isNaN (.parse js/Date string))))
