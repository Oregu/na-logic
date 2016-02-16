(ns nal.core
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic]))

;; Truth-value functions

(defn f_rev [fc1 fc2 fc]
  (fresh [f1 c1 f2 c2 f c]
    (== [f1 c1] fc1)
    (== [f2 c2] fc2)
    (== [f  c ] fc )
    (pred c1 #(< % 1))
    (pred c2 #(< % 1))
    (project [c1 c2 f1 f2]
      (== f (/ (+ (* (/ c1 (- 1 c1)) f1) (* (/ c2 (- 1 c2)) f2)) (+ (/ c1 (- 1 c1)) (/ c2 (- 1 c2)))))
      (== c (/ (+ (/ c1 (- 1 c1)) (/ c2 (- 1 c2))) (+ (/ c1 (- 1 c1)) (/ c2 (- 1 c2)) 1))))))

;; Revision

(defn revision [prem1 prem2 concl]
  (fresh [s t t1 t2]
    (== [s t1] prem1)
    (== [s t2] prem2)
    (== [s t ] concl)
    (f_rev t1 t2 t)))

;; Argument processing

(def include1)

(defn include [l1 l2]
  (nonlvaro l2)
  (include1 l1 l2)
  (!= l1 [])
  (!= l1 l2))

(defn include1 [l1 l2]
  (conde
    [(== l1 [])]
    [(fresh [h t1 t2]
      (conso h t1)
      (conso h t2)
      (include1 t1 t2))]
    [(fresh [h t1 t2]
      (conso h t1 l1)
      (conso h t2 l2)
      (include1 t1 t2))]
    [(fresh [h1 h2 t1 t2]
      (conso h1 t1 l1)
      (conso h2 t2 l2)
      (!= h1 h2)
      (include1 l1 t2))]))


;; Tests

(defn test-revision []
  (run 1 [r]
    (revision ['(inheritance bird swimmer) [1 0.8]] ['(inheritance bird swimmer) [0 0.5]] r)))
