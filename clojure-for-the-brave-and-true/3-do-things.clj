(ns brave-and-true)

;;;; SYNTAX --------------------------------------------------------------------

;;; Forms

1
"a string"
["a" "vector" "of" "strings"]

(+ 1 2 3)
(str "It was the panda " "in the library " "with a dust buster")

;;; Control Flow

;; if

(if true
  "By Zeus's hammer!"
  "By Aquaman's trident!")

(if false
  "By Zeus's hammer!"
  "By Aquaman's trident!")

(if false
  "By Odin's elbow!")

;; do

(if true
  (do (println "Success!")
      "By Zeus's hammer!")
  (do (println "Failure!")
      "By Aquaman's trident!"))

;; when

(when true
  (println "Success!")
  "abra cadabra")

;; nil, true, false, Truthiness, Equality, and Boolean expressions
;; Both nil and false are used to represent logical falsiness, all others are truthy

(nil? 1)
(nil? false)

(if "bears"
  "bears beets BSG")
(if nil
  "Thus won't be the result because nil is falsey"
  "nil is falsey")

(= 1 1)
(= nil nil)
(= 1 2)
(= [1 2] [1 2])

;; or returns either the first truthy value, or the last value

(or false nil :venti :large)
(or false false nil)
(or nil nil false)

;; and returns either the first falsey value, or the last value

(and :wifi :coffee)
(and :yes nil false :yup)

;;; Naming Values with def

(def failed-protagonist-names
  ["Larry Potter" "Doreen the explorer" "The Incredible Bulk"])

failed-protagonist-names
brave-and-true/failed-protagonist-names ; namespace defined with ns

(def severity :mild)
(def error-message "OH GOD! IT'S A DISASTER! WE'RE ")
(if (= severity :mild)
  (def error-message (str error-message "MILDLY INCONVENIENCED!"))
  (def error-message (str error-message "DOOOOOOOMED!")))
error-message

(defn error-message
  [severity]
  (str "OH GOD! IT'S A DISASTER! WE'RE "
       (if (= severity :mild)
         "MILDLY INCONVENIENCED!"
         "DOOOOOOOMED!")))
(error-message :mild)

;;;; DATA STRUCTURES -----------------------------------------------------------

;;; Numbers

93
1.2
1/5

;;; Strings

"Lord Voldemort"
"\"He who must not be named\""
"\"Great cow of Moscow!\" - Hermes Conrad"

;;; Maps

{}
{:first-name "Charlie"
 :last-name "McFishwich"}
{"string-key" +}
{:name {:first "John" :middle "Jacob" :last "Jingleheimerschmidt"}}

(hash-map :a 1 :b 2)

(get {:a 0 :b 1} :b)
(get {:a 0 :b {:c "ho hum"}} :b)
(get {:a 0 :b 1} :c)
(get {:a 0 :b 1} :c "Unicorns?")

(get-in {:a 0 :b {:c "ho hum"}} [:b :c])

({:name "The Human Coffeepot"} :name)

;;; Keywords

:a
:rumplestiltsken
:34
:_?

(:a {:a 1 :b 2 :c 3})
(get {:a 1 :b 2 :c 3} :a)

(:d {:a 1 :b 2 :c 3} "Not in map")

;;; Vectors

[3 2 1]
(vector "creepy" "full" "moon")

(get [3 2 1] 0)
(get ["a" {:name "Pugsley Winterbottom"} "c"] 1)


(conj [1 2 3] 4) ; Elements are added to the end of a vector

;;; Lists

'(1 2 3 4)

(nth '(:a :b :c) 0)
(nth '(:a :b :c) 2)

(list 1 "two" {3 4})

(conj '(1 2 3) 4) ; Elements are added to the beginning of a list

;;; Sets

#{"Kurt vonnegut" 20 :icicle}

(hash-set 1 1 2 2)

(conj #{:a :b} :b)

(set [3 3 3 4 4]) ; Can create from vectors or lists
(set '(3 3 3 4 4))

(contains? #{:a :b} :a)
(contains? #{:a :b} 3)
(contains? #{nil} nil)

(:a #{:a :b})
(#{:a :b} :a)

(get #{:a :b} :a)
(get #{nil :b} nil) ;=> nil
(get #{:a :b} "Vonnegut") ;=> nil, so contains? might be better

;;;; FUNCTIONS -----------------------------------------------------------------

;;; Calling functions

(+ 1 2 3 4)
(* 1 2 3 4)
(first [1 2 3 4])
(first '(1 2 3 4))

(or + -)
((or + -) 1 2 3)
((and (= 1 1) +) 1 2 3)
((first [+ 0]) 1 2 3)

(inc 1.1)
(map inc [0 1 2 3])

(+ (inc 199) (/ 100 (- 7 2)))
(+ 200 (/ 100 (- 7 2))) ; evaluated "(inc 199)"
(+ 200 (/ 100 5)) ; evaluated (- 7 2)
(+ 200 20) ; evaluated (/ 100 5)
220 ; final evaluation

;;; Function calls, macro calls, and special forms

(if good-mood
  (tweet walking-on-sunshine-lyrics)
  (tweet mopey-country-song-lyrics))

;;; Defining functions

(defn too-enthusiastic
  "Return a cheer that might be a bit too enthusiastic"
  [name]
  (str "OH. EM. GEE! " name " YOU ARE DEFINITELY LIKE THE BEST "
       "MAN SLASH WOMAN EVER I LOVE YOU AND WE SHOULD RUN AWAY SOMEWHERE"))

;; Parameters and arity

(defn no-params
  []
  "I take no parameters!")
(defn one-param
  [x]
  (str "I take one parameter: " x))
(defn two-param
  [x y]
  (str "I take two parameters and smoosh them: " x y))


(defn multi-arity
  ([p1 p2 p3]
   (str p1 p2 p3))
  ([p1 p2]
   (str p1 p2))
  ([p1]
   (str p1)))

(defn x-chop
  "Describe the kind of chop you're inflicting on someone"
  ([name chop-type]
   (str "I " chop-type " chop " name "! Take that!"))
  ([name]
   (x-chop name "karate")))

(x-chop "Kanye West" "slap")
(x-chop "Kanye East")

(defn weird-arity
  ([]
   "Destiny dressed you this morning, my friend, and now Fear is
trying to pull off your pants. If you give up, if you give in,
you're gonna end up naked with Fear just standing there laughing
at your dangling unmentionables! - the Tick")
  ([number]
   (inc number)))

(defn codger-communication
  [whippersnapper]
  (str "Get off my lawn, " whippersnapper "!!!"))

(defn codger
  [& whippersnappers]
  (map codger-communication whippersnappers))

(codger "Joe", "May")

(defn favorite-things
  [name & things]
  (str "Hi " name ", here are my favorite things: "
       (clojure.string/join ", " things)))

(favorite-things "Doreen" "gum" "shoes" "karate")

;; Destructuring

(defn my-first
  "Return the first element of a collection"
  [[first-thing]] ; Notice that first-thing is within a vector
  first-thing)

(my-first ["oven" "bike" "war-axe"])

(defn chooser
  [[first-choice second-choice & unimportant-choices]]
  (println (str "Your first choice is: " first-choice))
  (println (str "Your second choice is: " second-choice))
  (println (str "We're ignoring the rest of your choices. "
                "Here they are in case you need to cry over them: "
                (clojure.string/join ", " unimportant-choices))))

(chooser ["Marmalade", "Handsome Jack", "Pigpen", "Aquaman"])

(defn announce-treasure-location
  [{lat :lat lng :lng}]
  (println (str "Treasure lat: " lat))
  (println (str "Treasure lng: " lng)))

(announce-treasure-location {:lat 28.22 :lng 81.33})

(defn announce-treasure-location
  [{:keys [lat lng]}]
  (println (str "Treasure lat: " lat))
  (println (str "Treasure lng: " lng)))

(defn receive-treasure-location
  [{:keys [lat lng] :as treasure-location}]
  (println (str "Treasure lat: " lat))
  (println (str "Treasure lng: " lng))
  treasure-location)

(receive-treasure-location {:lat 28.22 :lng 81.33})

;; Function body

(defn illustrative-function
  []
  (+ 1 304)
  30
  "joe")

(illustrative-function)


(defn number-comment
  [x]
  (if (> x 6)
    "Oh my gosh! What a big number!"
    "That number's OK, I guess"))

(number-comment 5)
(number-comment 7)

;;; Anonymous functions

;(fn [param-list]
;  function-body)
 
(map (fn [name] (str "Hi " name))
     ["Voldemort" "Mr. Magoo"])

((fn [x] (* x 3)) 8)

(def my-special-multiplier (fn [x] (* 3 x)))
(my-special-multiplier 12)

#(* % 3) ; Anonymous function shorthand
(#(* % 3) 8)

(map #(str "Hi, " %)
     ["Darth Vader" "Mr. Magoo"])

(#(str %1 " and " %2) "cornbread" "butter beans") ; Can use %1 %2 etc 

(#(identity %&) 1 "blarg" :yip) ; %& for a rest parameter


;;; Returning functions

(defn inc-maker
  "Create a custom incrementor"
  [inc-by]
  #(+ % inc-by))

(def inc3 (inc-maker 3))

(inc3 7)

;;;; PUTTING IT ALL TOGETHER ---------------------------------------------------

(def asym-hobbit-body-parts
  [{:name "head" :size 3}
   {:name "left-eye" :size 1}
   {:name "left-ear" :size 1}
   {:name "mouth" :size 1}
   {:name "nose" :size 1}
   {:name "neck" :size 2}
   {:name "left-shoulder" :size 3}
   {:name "left-upper-arm" :size 3}
   {:name "chest" :size 10}
   {:name "back" :size 10}
   {:name "left-forearm" :size 3}
   {:name "abdomen" :size 6}
   {:name "left-kidney" :size 1}
   {:name "left-hand" :size 2}
   {:name "left-knee" :size 2}
   {:name "left-thigh" :size 4}
   {:name "left-lower-leg" :size 3}
   {:name "left-achilles" :size 1}
   {:name "left-foot" :size 2}])

(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(matching-part {:name "left-nostril" :size 97})

(defn symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (set [part (matching-part part)])))))))

(symmetrize-body-parts asym-hobbit-body-parts)

;;; let

(let [x 3]
  x)

(def dalmatian-list
  ["Pongo" "Perdita" "Puppy 1" "Puppy 2"])

(let [dalmatians (take 2 dalmatian-list)]
  dalmatians)

(def x 0)
(let [x 1] x)
(let [x (inc x)] x)

(let [[pongo & dalmatians] dalmatian-list] ; Can use destructuring in let
  [pongo dalmatians])
 
(let [a 12 ; Multiple let arguments
      b 32]
  (+ a b))


