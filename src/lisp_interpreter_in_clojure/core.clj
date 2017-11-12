(ns lisp-interpreter-in-clojure.core)

(defn my-eval [exp env]
  ;(println (first exp))
  (cond
    (symbol? exp) (env exp)
    (number? exp) exp
    (= 'define (first exp)) (assoc env (second exp) (nth exp 2))

    :else "syntax error"))
