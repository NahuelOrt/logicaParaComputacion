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
  
  
-- EJERCICIO 1.2 --
--a)
fa :: L
fa = undefined
--b)
fb :: L
fb = undefined
--c)
fc :: L
fc = undefined
--d)
fd :: L
fd = undefined


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
valores (Bin l1 bc l2) = (valores l1) ++ (valores l2)

--c)
dobleNeg :: L -> L
dobleNeg = undefined

--d)
cambiar :: L -> L
cambiar = undefined

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