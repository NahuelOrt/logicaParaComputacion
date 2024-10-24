
module Lab3Solution where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: 
-- Números: 
----------------------------------------------------

import Prelude
import Data.List
import Data.Maybe

----------------------------------------------------------------------------------
-- Formalización del lenguaje y otros elementos
----------------------------------------------------------------------------------
type Var = String
type Lit = (Var,Bool)
type I = [Lit]

data L = V Var | Neg L | Bin L BC L
  deriving (Eq)

data BC = And | Or | Imp | Iff 
  deriving (Show, Eq)

data Clase = Tau | Contra | Cont
  deriving (Show, Eq)

data Consecuencia = [L] :|= L 
  deriving (Show, Eq)  

data Tableau = Hoja I | Conj [L] Tableau | Dis [L] Tableau Tableau
   
top = Bin (V "p") Or  (Neg $ V "p") 
bot = Bin (V "p") And (Neg $ V "p") 

-- 1)
-- Pre: recibe una lista de asignaciones de valores de verdad sobre variables
-- Pos: retorna True si y solo si la lista es consistente, o sea representa una interpretación
esConsistente :: I -> Bool
esConsistente [] = True
esConsistente ((v,b):vs) = (not (elem (v, not b) vs)) && (esConsistente vs)

-- 2)
-- Pre: recibe una interpretación dada como lista de asignaciones (no vacía y consistente) 
-- Pos: retorna la interpretación expresada como una conjunción de literales
int2f :: I -> L
int2f i = conj (map lit2f i)

lit2f :: Lit -> L
lit2f (a,b) 
        | b = V a
        | otherwise = Neg (V a)

conj :: [L] -> L
conj [] = top
conj [x] = x
conj (x:xs) = Bin x And (conj xs)

-- 3)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna el tableau de f
tableau :: L -> Tableau
tableau f = tableauRec [f]
  where
    tableauRec (x:xs) = case x of {
      --Dis
      Bin f1 Or f2 -> Dis (x:xs) (tableauRec (f1:xs)) (tableauRec (f2:xs));
      Neg (Bin f1 And f2) -> Dis (x:xs) (tableauRec ((Neg f1):xs)) (tableauRec ((Neg f2):xs));
      Bin f1 Imp f2 -> Dis (x:xs) (tableauRec ((Neg f1):xs)) (tableauRec (f2:xs));
      Neg (Bin f1 Imp f2) -> Dis (x:xs) (tableauRec (f1:xs)) (tableauRec ((Neg f2):xs));
      Bin f1 Iff f2 -> Dis (x:xs) (tableauRec ((f1):(f2):xs)) (tableauRec ((Neg f1):(Neg f2):xs));
      Neg (Bin f1 Iff f2) -> Dis (x:xs) (tableauRec (f1:(Neg f2):xs)) (tableauRec ((Neg f1):f2:xs));
      --Conj
      Bin f1 And f2 -> Conj (x:xs) (tableauRec (f1:f2:xs));
      Neg (Bin f1 Or f2) -> Conj (x:xs) (tableauRec ((Neg f1):(Neg f2):xs));
      --Hoja
      (V y) -> if (allLit xs) then (Hoja (map f2int ((V y):xs))) else (tableauRec ((fstNonLit xs):(V y):(xs \\ [fstNonLit xs])));
      Neg (V y) -> if (allLit xs) then (Hoja (map f2int ((Neg (V y)):xs))) else (tableauRec ((fstNonLit xs):(Neg (V y)):(xs \\ [fstNonLit xs])));
      Neg (Neg f1) -> tableauRec (f1:xs);
    }

f2int :: L -> Lit
f2int (V x) = (x, True)
f2int (Neg (V x)) = (x, False)

fstNonLit :: [L] -> L
fstNonLit [] = error "No existe"
fstNonLit ((V _):fs) = fstNonLit fs
fstNonLit ((Neg (V _)):fs) = fstNonLit fs
fstNonLit (x:xs) = x

allLit :: [L] -> Bool
allLit [] = True
allLit ((V _):fs) = allLit fs
allLit ((Neg (V _)):fs) = allLit fs
allLit (x:xs) = False

-- 4)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna True si y solo si f es sat
sat :: L -> Bool
sat = undefined
 
-- 5)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna una lista con todos los modelos completos de f
-- Recomendación: para imprimirlos los modelos en lineas distintas:
--                ghci> mapM_ print $ modelos f
modelos :: L -> [I]
modelos = undefined

-- 6)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna la clase semántica a la que pertenece f
clasificar :: L -> Clase
clasificar = undefined

-- 7)
-- Pre: recibe una consecuencia
-- Pos: retorna la consecuencia expresada como una fórmula de LP
cons2f :: Consecuencia -> L
cons2f = undefined

-- 8)     
-- Pre: recibe una consecuencia
-- Pos: retorna True si y solo si la consecuencia es válida
valida :: Consecuencia -> Bool
valida = undefined

-- 9)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna f en FND
fnd :: L -> L
fnd = undefined

-- 10)
-- Pre: recibe una fórmula f de LP
-- Pos: retorna f en FNC
fnc :: L -> L
fnc = undefined


----------------------------------------------------------------------------------
-- Fórmulas del Lab1 para probar
----------------------------------------------------------------------------------
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


----------------------------------------------------------------------------------
-- Algunas funciones auxiliares 
----------------------------------------------------------------------------------

vars :: L -> [Var]
vars (V p)         = [p]
vars (Neg f)       = vars f
vars (Bin f1 _ f2) = nub (vars f1 ++ vars f2)

----------------------------------------------------------------------------------
-- Pretty Printing
----------------------------------------------------------------------------------
instance Show L where
  show (V p)           = p
  show (Neg (Neg a))   = "¬" ++ show (Neg a)
  show (Neg (V p))     = "¬" ++ show (V p)
  show (Neg a)         = "¬" ++ show a ++ ""
  show (Bin a And b)   = "(" ++ show a ++ " /\\ " ++ show b ++ ")"
  show (Bin a Or b)    = "(" ++ show a ++ " \\/ " ++ show b ++ ")"
  show (Bin a Imp b)   = "(" ++ show a ++ " --> " ++ show b ++ ")"
  show (Bin a Iff b)   = "(" ++ show a ++ " <-> " ++ show b ++ ")"

instance Show Tableau where
    show = prettyPrintT  

-- Formatear tableau indentado
-- Adaptado de https://stackoverflow.com/a/19442407
prettyPrintT :: Tableau -> String
prettyPrintT t = unlines (prettyPrintAux t)
  where
    prettyPrintAux (Hoja i)       = [show (map lit2f i) ++ if esConsistente i then " O" else " X"]
    prettyPrintAux (Conj l t)     = (show l) : prettyPrintSubTree [t]
    prettyPrintAux (Dis  l t1 t2) = (show l) : prettyPrintSubTree [t1,t2]
    --
    prettyPrintSubTree []     = []
    prettyPrintSubTree [t]    = ((pad "'- " "   ") (prettyPrintAux t))
    prettyPrintSubTree (t:ts) = ((pad "+- " "|  ") (prettyPrintAux t)) ++ prettyPrintSubTree ts
    --
    pad first rest = zipWith (++) (first : repeat rest)
    --
    lit2f (v,b) | b = V v 
                | otherwise = Neg (V v)