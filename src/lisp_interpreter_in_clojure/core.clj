(ns lisp-interpreter-in-clojure.core)

(declare my-eval)

(defn tagged-list? [exp tag]
  (and (list? exp) (= (first exp) tag)))

(defn self-evaluating? [exp]
  (or (number? exp) (string? exp)))

(defn variable? [exp]
  (symbol? exp))

(defn definition? [exp]
  (tagged-list? exp 'define))

(defn eval-definition [exp env]
  (let [symbol (second exp)
        value (my-eval (nth exp 2) env)]
    (swap! env assoc symbol value)))

(defn do? [exp]
  (tagged-list? exp 'do))

(defn eval-do [exps env]
  (loop [rest-exps exps env env]
    (if (= (count rest-exps) 1)
      (my-eval (first rest-exps) env)
      (do
        (my-eval (first rest-exps) env)
        (recur (rest rest-exps) env)))))

(defn my-eval [exp env]
  (cond
    (self-evaluating? exp) exp
    (variable? exp) ((deref env) exp)
    (do? exp) (eval-do (rest exp) env)
    (definition? exp) (eval-definition exp env)
    :else "syntax error"))

(defn lisp [exp]
  (my-eval exp (atom {})))