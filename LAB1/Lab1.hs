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
cantPropX (V x) y
      | x == y = 1
      | otherwise = 0
cantPropX (Neg l) y = cantPropX l y
cantPropX (Bin l1 bc l2) y = (cantPropX l1 y) + (cantPropX l2 y)

--f)
listarProp :: L -> [Var]
listarProp (V x) = [x]
listarProp (Neg l) = listarProp l 
listarProp (Bin l1 bc l2) = concatSinRep (listarProp l1) (listarProp l2)

concatSinRep :: [Var] -> [Var] -> [Var]
concatSinRep list1 [] = list1
concatSinRep list1 (y:ys) = concatSinRep (agregarVarNoRep list1 y) ys

agregarVarNoRep :: [Var] -> Var -> [Var]
agregarVarNoRep [] y = [y]
agregarVarNoRep (x:xs) y 
      | x == y = x:xs
      | otherwise = x : agregarVarNoRep xs y

--g)
sustCon :: L -> BC -> BC -> L
sustCon (V x) b1 b2 = (V x)
sustCon (Neg l) b1 b2 = Neg (sustCon l b1 b2)
sustCon (Bin l1 bc l2) b1 b2
      | bc == b1 = Bin (sustCon l1 b1 b2) b2 (sustCon l2 b1 b2)
      | otherwise = Bin (sustCon l1 b1 b2) bc (sustCon l2 b1 b2)


--h)
swapCon :: L -> BC -> BC -> L
swapCon (V x) b1 b2 = (V x)
swapCon (Neg l) b1 b2 = Neg (swapCon l b1 b2)
swapCon (Bin l1 bc l2) b1 b2
      | bc == b1 = Bin (swapCon l1 b1 b2) b2 (swapCon l2 b1 b2)
      | bc == b2 = Bin (swapCon l1 b1 b2) b1 (swapCon l2 b1 b2)
      | otherwise = Bin (sustCon l1 b1 b2) bc (sustCon l2 b1 b2)

--i)
invertir :: L -> L
invertir (V x) = (Neg (V x))
invertir (Neg (V x)) = (V x)
invertir (Neg l) = dobleNeg (Neg (invertir l))
invertir (Bin l1 bc l2) = swapCon (Bin (invertir (swapCon l1 And Or)) bc (invertir (swapCon l2 And Or))) And Or

--j)
sustSimp :: Var -> L -> L -> L
sustSimp x l1 (V y)
      | x == y = l1
      | otherwise = (V y)
sustSimp x l1 (Neg l2) = (Neg (sustSimp x l1 l2))
sustSimp x l1 (Bin l2 bc l3) = Bin (sustSimp x l1 l2) bc (sustSimp x l1 l3)

--k)
sustMult :: [(Var, L)] -> L -> L
sustMult [] (V y) = (V y)
sustMult ((x, l1):xs) (V y)
      | x == y = l1
      | otherwise = sustMult xs (V y)
sustMult [] (Neg l) = (Neg l)
sustMult list (Neg l) = Neg (sustMult list l)
sustMult [] (Bin l1 bc l2) = (Bin l1 bc l2)
sustMult list (Bin l1 bc l2) = Bin (sustMult list l1) bc (sustMult list l2)