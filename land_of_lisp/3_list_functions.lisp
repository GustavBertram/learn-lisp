;; CONS exercises

(cons 'chicken 'cat)
(cons 'dog '(chicken . cat))
(cons 'cow '(duck goose))
(cons 'chicken ())

;; CAR / CDR exercises

(car (cdr '(pork beef chicken)))
(cadr '(pork beef chicken))

(car '((peas carrots tomatoes) (pork beef chicken)))
(cdr (car '((peas carrots tomatoes) (pork beef chicken))))
(cdar '((peas carrots tomatoes) (pork beef chicken)))

(cddr '((peas carrots tomatoes) (pork beef chicken) duck))
(caddr '((peas carrots tomatoes) (pork beef chicken) duck))
(cddar '((peas carrots tomatoes) (pork beef chicken) duck))
(cadadr '((peas carrots tomatoes) (pork beef chicken) duck))




