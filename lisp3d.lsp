;(load 'lisp3d) -> per carregar es fitxer

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
    (cons nom 'figures)
    "Figura creada"
)