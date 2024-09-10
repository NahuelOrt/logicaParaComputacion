module Lab1 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: Nahuel Pereyra 
-- Números: 254438
----------------------------------------------------

import Prelude

-- EJERCICIO 1.1 --
type Var = String

data L = V Var | Neg L | Bin L BC L
  deriving (Show, Eq)
data BC = And | Or | Imp | If
  deriving (Show, Eq)
  
p :: L 
p = (V "p")

q :: L 
q = (V "q")

r :: L 
r = (V "r")

s :: L 
s = (V "s")
  
-- EJERCICIO 1.2 --
--a)
fa :: L
fa = Bin (V "p") And (Neg (Neg (V "q")))
--b)
fb :: L
fb = Bin (V "p") And (Bin (Neg (V "q")) And (Neg (V "r")))
--c)
fc :: L
fc = Bin (Neg (Neg (V "p"))) Or (Neg (Bin (V "q") And (V "p")))
--d)
fd :: L
fd = Bin (Neg (Bin (V "r") Imp (V "r"))) And (Bin (Neg (Neg (V "p"))) Or (Neg (Bin (V "q") And (V "p"))))


-- EJERCICIO 1.3 --
--a)
cantBin :: L -> Int
cantBin (V x) = 0
cantBin (Neg l) = cantBin l
cantBin (Bin l1 b l2) = 1 + cantBin l1 + cantBin l2

--b)
valores :: L -> [(Var,Bool)]
valores (V x) = [(x, True)]
valores (Neg (V y)) = [(y, False)]
valores (Neg l) = valores l
valores (Bin l1 bc l2) = valores l1 ++ valores l2

--c)
dobleNeg :: L -> L
dobleNeg (V x) = (V x)
dobleNeg (Neg (V y)) = (Neg (V y))
dobleNeg (Neg (Neg l)) = l
dobleNeg (Bin l1 bc l2) = (Bin (dobleNeg l1) bc (dobleNeg l2))

--d)
cambiar :: L -> L
cambiar (V x) = (V x)
cambiar (Neg l) = (Neg (cambiar l))
cambiar (Bin l1 Or l2) = (Bin (Neg (cambiar l1)) Imp (cambiar l2))
cambiar (Bin m1 bc m2) = (Bin (cambiar m1) bc (cambiar m2))

--e)
cantPropX :: L -> Var -> Int
cantPropX = undefined

--f)
listarProp :: L -> [Var]
listarProp = undefined

--g)
sustCon :: L -> BC -> BC -> L
sustCon = undefined

--h)
swapCon :: L -> BC -> BC -> L
swapCon = undefined

--i)
invertir :: L -> L
invertir = undefined

--j)
sustSimp :: Var -> L -> L -> L
sustSimp = undefined

--k)
sustMult :: [(Var, L)] -> L -> L
sustMult = undefined