(ns learnclojure)

(type 1)
(type :a) ;keyword
(type 'a) ;symbol

(type ())
(type [])
(type {})
(type #{})

(defmacro dummy [x] `(list foo ~x))
(macroexpand '(dummy foo))

; Doesn't work for lists
([1 2 3] 1)
(:a {:a 2 :b 3})
({:a 2 :b 3} :a)
(#{:a 2 :b 3} :c)
(:c #{:a 2 :b 3})

(def x "Hello")

(let [x "Steve"]
  (print "Hello," x))

(print x)

(do
  (print "Hi")
  (print "Hello")
  :ok)

;;; Control Flow

;; if if-not when when-not (no unless!)

(when-not false
          :ok)


(case x
    "Goodbye" :goodbye
    "Hello" :hello
    :nothing)

(cond
  (= 1 2) :noooo
  (= (reverse x) "olleH") :olleh
  :otherwise :nothing)

(apply str (reverse x))


;;; Functions

(fn [] "Hello")
((fn [] "Hello"))
(def hello (fn [] "Hello"))
(hello)
(#(str "Hello"))
(defn moo "The docstring is in an odd place."
  []
  "Test")

;; To use a docstring
(require '[clojure.repl :refer [doc]])
(doc moo)

(defn hello
  ([] "Hello, uh, you")
  ([name] (str "Hello, " name)))

(hello)
(hello "Joe")

(defn hello
  ([] (hello "uh, you"))
  ([name] (str "Hello, " name)))


(defn hello [config]
  (str "Hi, " (:name config)))

(hello {:name "Joe"})

(defn hello [{name :name}]
  (str "Hi, " name))

(hello {:name "Joe"})

(defn hello [[name title]]
  (str "Hi, " title " " name))

(hello ["Jane" "Captain"])



