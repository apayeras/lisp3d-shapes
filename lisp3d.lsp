; (load 'lisp3d-shapes/lisp3d) -> per carregar es fitxer
; Autors: Antoni Payeras
;         Mauricio Gallardo
;         José Ramón Muñoz

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
    "Figura borrada"
)

(defun borra-figures
    (putprop 'escena nil 'figures)
    (cls)
)

(defun pinta-figura (f)
    "Pintada"
)

(defun cls-figura (f)
    (putprop f '(255 255 255) 'color)
    (pinta-figura f)
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
(t (* (car l) (mult (cdr l)))))) 

; funcion producto escalar
(defun producteEscalar (l)
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