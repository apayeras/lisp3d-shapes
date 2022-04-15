; (load 'lisp3d-shapes/lisp3d) -> per carregar es fitxer
; Autors: Antoni Payeras
;         Mauricio Gallardo
;         José Ramón Muñoz

;; Pensar com borrar una figura de la pantalla sense alterar el seu color original +- Arreglat
;; Pensar en si borram una figura de l'escena ja no la podem tornar a pintar - revisar octaedre no se pinta bé xd

(defun inicia-patrons ()
    (putprop 'cub '((-0.5 0 -0.5)(0.5 0 -0.5)(0.5 0 0.5)(-0.5 0 0.5)(-0.5 1 -0.5)(0.5 1 -0.5)(0.5 1 0.5)(-0.5 1 0.5)) 'punts)
    (putprop 'cub '((1 2)(2 3)(3 4)(4 1)(1 5)(2 6)(3 7)(4 8)(5 6)(6 7)(7 8)(8 5)) 'arestes)
    (putprop 'cub '((1 2 3 4)(1 5 9 6)(2 6 7 10)(3 8 11 7)(5 4 8 12)(9 10 11 12)) 'cares)
    (putprop 'prisma '((-0.5 0 -0.5)(-0.5 0 0.5)(0 0 0.5)(-0.5 1 -0.5)(-0.5 1 0.5)(0 1 0.5)) 'punts)
    (putprop 'prisma '((1 2)(2 3)(3 1)(1 4)(2 5)(3 6)(4 5)(5 6)(4 6)) 'arestes)
    (putprop 'prisma '((1 2 3)(7 8 9)(1 4 5 7)(2 5 6 8)(3 4 9 6)) 'cares)
    (putprop 'octaedre '((-0.5 0.5 -0.5)(0.5 0.5 -0.5)(0.5 0.5 0.5)(-0.5 0.5 0.5)(0 0 0)(0 1 0)) 'punts)
    (putprop 'octaedre '((1 2)(2 3)(3 4)(4 1)(1 5)(2 5)(3 5)(4 5)(1 6)(2 6)(3 6)(4 6)) 'arestes)
    (putprop 'octaedre '((1 2 3 4)(1 2 5)(2 3 5)(3 4 5)(1 4 5)(1 2 6)(2 3 6)(3 4 6)(1 4 6)) 'cares)
    "Patrons iniciats"
)

(defun crea-figura (nom patro color)
    (putprop nom patro 'patro)
    (putprop nom color 'color)
    (putprop nom '((1 0 0 0)(0 1 0 0)(0 0 1 0)(0 0 0 1)) 'matriu)
    (putprop 'escena (cons nom (get 'escena 'figures)) 'figures)
    "Figura creada"
)

(defun borra (x l)
    (cond ((null l) nil)
        ((equal x (car l)) (cdr l))
        (t (cons (car l) (borra x (cdr l))))
    )
)

(defun borra-figura (f)
    (cls-figura f)
    (putprop 'escena (borra f (get 'escena 'figures)) 'figures)
    "Figura eliminada"
)

(defun borra-figures ()
    (putprop 'escena nil 'figures)
    (cls)
)

(defun pinta-aresta (a f)
    (move (realpart (round (+ (* (car (mult-vec-matriu (snoc '1 (agafa-element (car a) (get (get f 'patro) 'punts))) (get f 'matriu))) 40) 320)))
    (realpart (round (+ (* (cadr (mult-vec-matriu (snoc '1 (agafa-element (car a) (get (get f 'patro) 'punts))) (get f 'matriu))) 40) 187))))
    (draw (realpart (round (+ (* (car (mult-vec-matriu (snoc '1 (agafa-element (cadr a) (get (get f 'patro) 'punts))) (get f 'matriu))) 40) 320)))
    (realpart (round (+ (* (cadr (mult-vec-matriu (snoc '1 (agafa-element (cadr a) (get (get f 'patro) 'punts))) (get f 'matriu))) 40) 187))))
)


(defun pinta-arestes (a f)
    (cond ((null (car a)) nil)
        (t (pinta-aresta (agafa-element (car a) (get (get f 'patro) 'arestes)) f) (pinta-arestes (cdr a) f))
    )
)

(defun pinta-cares (c f)
    (cond ((null (car c)) nil)
          (t (pinta-arestes (car c) f) (pinta-cares (cdr c) f))
    )
)

(defun pinta-figura (f)
    (eval (cons 'color (get f 'color)))
    (pinta-cares (get (get f 'patro) 'cares) f)
    (color 0 0 0)
    "Pintada"
)

(defun cls-figura (f)
    (putprop f (get f 'color) 'color-pre)
    (putprop f '(255 255 255) 'color)
    (pinta-figura f)
    (putprop f (get f 'color-pre) 'color)
    "Figura esborrada"
)

(defun snoc (x l) 
    (cond ((null l) (cons x l))
        (t (cons (car l) (snoc x (cdr l))))
    )
)

(defun agafa-element (num l)
    (cond ((= num 1) (car l))
        (t (agafa-element (- num 1) (cdr l)))

    )
)

(defun afegir-punts (l)
    (cond ((null (cdr l)) (cons (snoc '1 (car l)) nil))
        (t (cons (snoc '1 (car l)) (afegir-punts (cdr l))))
    )
)


(defun mult-vec-matriu (l1 l2t)
    (cond ((null (cdr l2t)) (cons (producte-escalar (cons l1 (cons (car l2t) nil))) nil))
        (t (cons(producte-escalar (cons l1 (cons (car l2t) nil))) (mult-vec-matriu l1 (cdr l2t))))
    )
    
)

(defun mult-matriu (l1 l2)
    (cond ((null (cdr l1)) (cons (mult-vec-matriu (car l1) (transposta l2)) nil))
        (t (cons (mult-vec-matriu (car l1) (transposta l2)) (mult-matriu (cdr l1) l2)))
    )
)

; convertir grados a radianes
(defun grausRadians (numberOfDegrees) 
(* PI (/ numberOfDegrees 180.0))
)

; definir matriz de translación
(defun translacio (dx dy dz) 
    '((1 0 0 0) (0 1 0 0) (0 0 1 0) (dx dy dz 1))
)
; definir matriz de escalado
(defun escalat (dx dy dz)
    '((dx 0 0 0) (0 dy 0 0) (0 0 dz 0) (0 0 0 1))
)
; definir rotación eje x
(defun rotax (a)
    '((1 0 0 0) (0 cos(grausRadians(a)) (- 0 sin(grausRadians(a))) 0) 
    (0 sin(grausRadians(a)) cos(grausRadians(a)) 0) (0 0 0 1))
)
; definir rotación eje y
(defun rotay (a)
    '((cos(grausRadians(a)) 0 (- 0 sin(grausRadians(a))) 0) (0 1 0 0)
     (sin(grausRadians(a)) 0 cos(grausRadians(a)) 0) (0 0 0 1))
)
; definir rotación eje z
(defun rotaz (a)
    '((cos(grausRadians(a)) (- 0 sin(grausRadians(a))) 0 0)
     (sin(grausRadians(a)) (cos(grausRadians(a)) 0 0)
      (0 0 1 0) (0 0 0 1)))
)

; PRE MULTIPLICACIÓN

; función traspuesta 
(defun transposta (l)
(cond ((null (car l)) nil)
(t (cons (mapcar 'car l)
(transposta (mapcar 'cdr l))))))

; funcion multiplicar
(defun mult (l)
(cond ((null l) 1)
(t (* (car l) (mult (cdr l)))))
)

; funcion producto escalar
(defun producte-escalar (l)
    (apply '+ (mapcar 'mult (transposta l)))
)

; traspuesta de la matriz de translacion (2ª)
; producto escalar de la primera matriz con la segunda
(defun trasllada-figura (f x y z)

)

; traspuesta de la matriz de translacion (2ª)
; producto escalar de la primera matriz con la segunda
(defun rota-figura (f x y z)

)

; traspuesta de la matriz de translacion (2ª)
; producto escalar de la primera matriz con la segunda
(defun escala-figura (f x y z)

)

; traspuesta de la matriz de translacion (2ª)
; producto escalar de la primera matriz con la segunda
(defun inicia-figura (f)

)

