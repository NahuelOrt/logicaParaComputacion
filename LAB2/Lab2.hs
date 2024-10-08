module Lab2 where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: Nahuel Pereyra
-- Números: 254438
----------------------------------------------------

import Prelude

-- Formalización del lenguaje
type Var = String

data L = V Var | Neg L | Bin L BC L
  deriving (Eq)
data BC = And | Or | Imp | Iff
  deriving (Eq)

-- Fórmulas del Lab1
p = V "p"
q = V "q"
r = V "r"
fa :: L
fa = Bin p And (Neg (Neg q))                   -- (p ∧ ¬¬q)
fb :: L
fb = Bin p And (Bin (Neg q) And (Neg r))       -- (p ∧ ¬q ∧ ¬r)
fc :: L
fc = Bin (Neg (Neg p)) Or (Neg (Bin q And p))  -- (¬¬p ∨ ¬(q ∧ p))
fd :: L
fd = Bin (Neg (Bin r Imp r)) And fc            -- ¬(r ⊃ r) ∧ (¬¬p ∨ ¬(q ∧ p))




-- EJERCICIO 1 --
--1.1)
eval :: (Var -> Bool) -> L -> Bool
eval i (V x) = i x
eval i (Neg l) = not (eval i l)
eval i (Bin l1 bc l2) 
      | bc == And = (eval i l1) && (eval i l2)
      | bc == Or = (eval i l1) || (eval i l2)
      | bc == Imp = (eval i (Neg l1)) || (eval i l2)
      | bc == Iff = (eval i l1) == (eval i l2) -- (¬A∧¬B)∨(A∧B) ((not (eval i l1)) && (not (eval i l2))) || (( (eval i l1)) && ((eval i l2)))

--1.2)
itodasverdaderas ::  Var -> Bool
itodasverdaderas _ = True

--1.3)
itodasfalsas :: Var -> Bool
itodasfalsas _ = False

--1.4)
irfalsa :: Var -> Bool
irfalsa x
      | x == "r" = False
      | otherwise = True

--1.5)
-- Completar con verdadera/falsa:
-- fa es False bajo itodasfalsas           eval itodasfalsas fx
-- fb es False bajo itodasfalsas
-- fc es True bajo itodasfalsas
-- fd es False bajo itodasfalsas
-- 
-- fa es True bajo itodasverdaderas        eval itodasverdaderas fx
-- fb es False bajo itodasverdaderas
-- fc es True bajo itodasverdaderas
-- fd es False bajo itodasverdaderas
--
-- fa es True bajo irfalsa                 eval irfalsa fx
-- fb es False bajo irfalsa
-- fc es True bajo irfalsa
-- fd es False bajo irfalsa

--1.6)
creari :: [(Var, Bool)] -> (Var -> Bool)
creari ((v,b):is) x
          | x == v = b
          | otherwise = creari is x

--1.7)
-- Creo que no es igual a la interpretación del punto 4 ya que irfalsa comprende mucho mas 
-- que r, p y q. Es decir que si tenemos en cuenta alguna variable mas que no esté en esta
-- lista, puede ser False. Es decir que deja abierta la opcion de por ejemplo un (s, False).
-- Si en creari agregaramos un caso base para cuando is = [], entonces dependería de este,
-- es decir, si fuera creari [] _ = True, si podriamos afirmar la igualdad, y si fuera de
-- la forma creari [] _ = False podriamos afirmar la desigualdad.


-- EJERCICIO 2 --
type Fila = [(Var, Bool)]
type TV = [(Fila, Bool)]

data Clase = Tau | Contra | Cont | Sat | Fal

--2.1)
filas :: [Var] -> [Fila]
filas [] = [[]]
filas (v:vs) = [[(v,b)] ++ rs | b <- [False, True], rs <- filas vs]

--2.2)
tv :: L -> TV
tv l = [(f, eval (creari(f)) l) | f <- filas (listarProp (l))]

--Este es el ejercicio f de Lab1 (Lista las prop de L sin repeticiones.)
listarProp :: L -> [Var]
listarProp (V x) = [x]
listarProp (Neg l) = listarProp l 
listarProp (Bin l1 bc l2) = concatSinRep (listarProp l1) (listarProp l2)

--Esta es una funcion aux del ej f del Lab1
--f - aux 1)
concatSinRep :: [Var] -> [Var] -> [Var]
concatSinRep list1 [] = list1
concatSinRep list1 (y:ys) = concatSinRep (agregarVarNoRep list1 y) ys

--Esta es una funcion aux del ej f del Lab1
--f - aux 2)
agregarVarNoRep :: [Var] -> Var -> [Var]
agregarVarNoRep [] y = [y]
agregarVarNoRep (x:xs) y 
      | x == y = x:xs
      | otherwise = x : agregarVarNoRep xs y


--2.3) -- Tau | Contra | Cont | Sat | Fal
es :: L -> Clase -> Bool
es l Tau = esTau (tv l)
es l Contra = esContra (tv l)
es l Cont = (not (esTau (tv l))) && (not (esContra (tv l)))
es l Sat = not (esContra (tv l))
es l Fal = esFal (tv l)

--De aquí hasta el ejercicio 2.4 se declaran funciones auxiliares para poder utilizar en la 2.3.
--Dada una tv, evalua si es Tau
esTau :: TV -> Bool 
esTau [] = True
esTau ((f,b):xs) = b && esTau xs

--Dada una tv, evalua si es Contra
esContra :: TV -> Bool
esContra [] = True
esContra ((f,b):xs) = (not b) && esContra xs

--Dada una tv, evalua si es Sat
esSat :: TV -> Bool
esSat [] = False
esSat ((f,b):xs) = b || esSat xs

--Dada una tv, evalua si es Fal
esFal :: TV -> Bool
esFal [] = False
esFal ((f,b):xs) = (not b) || esFal xs

--2.4)
-- Completar con tautología/contingencia/contradicción:
-- fa es contingencia
-- fb es contingencia
-- fc es tautologia
-- fd es contradicción

--2.5) 
fnc :: L -> L
fnc l 
      | (es l Tau) = l
      | otherwise = crearConju (busquedaFalse (tv l))

busquedaFalse :: TV -> TV
busquedaFalse [] = []
busquedaFalse ((f,b):ts) 
      | b == True = busquedaFalse ts
      | otherwise = (f,b):(busquedaFalse ts)

crearDisyu :: Fila -> L
crearDisyu ((v,b):[])
      | b == False = (V v)
      | otherwise = (Neg (V v))
crearDisyu ((v,b):fs)
      | b == False = Bin (V v) Or (crearDisyu fs)
      | otherwise = Bin (Neg (V v)) Or (crearDisyu fs)

crearConju :: TV -> L
crearConju ((f,b):[]) = crearDisyu f
crearConju ((f,b):ts) = Bin (crearDisyu f) And (crearConju ts)

--Un test da fail y por lo que llegue a ver es por tema de eliminar implicaciones.

----------------------------------------------------------------------------------
-- Pretty Printing (rudimentario)
----------------------------------------------------------------------------------
instance Show L where
  show (V p)         = p
  show (Neg (Neg a)) = "¬" ++ show (Neg a)
  show (Neg (V p))   = "¬ " ++ show (V p)
  show (Neg a)       = "¬ (" ++ show a ++ ")"
  show (Bin a And b) = "(" ++ show a ++ ") /\\ (" ++ show b ++ ")"
  show (Bin a Or b)  = "(" ++ show a ++ ") \\/ (" ++ show b ++ ")"
  show (Bin a Imp b) = "(" ++ show a ++ ") --> (" ++ show b ++ ")"
  show (Bin a Iff b) = "(" ++ show a ++ ") <-> (" ++ show b ++ ")"