
module Lab4Solution where
------------------- Estudiante/s -------------------
-- Nombres y apellidos: 
-- Números: 
----------------------------------------------------

import Prelude
import Data.List

-- import Lab3   -- Importar el Laboratorio 3

type Nat = Int

----------------------------------------------------------------------------------
-- 1. Veraces y mentirosos
----------------------------------------------------------------------------------

-- Variables proposicionales:
-- ... COMPLETAR AQUÍ ... 

-- Respuesta: ... COMPLETAR AQUÍ ... 
ej1 :: L
ej1 = undefined                          

-- Respuesta: ... COMPLETAR AQUÍ ... 
ej2 :: L
ej2 = undefined
 
-- Respuesta: ... COMPLETAR AQUÍ ... 
ej3 :: L
ej3 = undefined


----------------------------------------------------------------------------------
-- 2. Crimen de tía Agatha
----------------------------------------------------------------------------------

-- 2.1) Respuesta: ... COMPLETAR AQUÍ ...    
agatha :: L
agatha = undefined


----------------------------------------------------------------------------------
-- 3. Coloreo de mapas
----------------------------------------------------------------------------------
-- 3.1)
type G = [(Nat,Nat)]

-- Pre: recibe un mapa representado como grafo (g), la cantidad de zonas (n) en el mapa 
--      y la cantidad de colores (m)
-- Pos: retorna una fórmula de LP formalizando el problema de colorear 
--      el mapa de n zonas con m colores
coloreo :: G -> Nat -> Nat -> L
coloreo g n m = undefined
  

-- 3.2)
-- Mapa de America Latina
latam :: G
latam = undefined
-- Referencia:
-- 1 = Argentina 
-- 2 = Bolivia 
-- 3 = Brasil 
-- 4 = Chile 
-- 5 = Colombia 
-- 6 = Ecuador 
-- 7 = Guayana 
-- 8 = Guayana Francesa 
-- 9 = Paraguay 
-- 10 = Perú 
-- 11 = Surinam 
-- 12 = Uruguay 
-- 13 = Venezuela
                  
-- 3.3.a) ... COMPLETAR AQUÍ ...

-- 3.3.b) ... COMPLETAR AQUÍ ...

-- 3.3.c) ... COMPLETAR AQUÍ ...


----------------------------------------------------------------------------------
-- Funciones sugeridas
----------------------------------------------------------------------------------

-- Conjuntoria (universal finito) de fórmulas indexadas
bigAnd :: [Int] -> (Int -> L) -> L
bigAnd is f = undefined

-- Disyuntoria (existencial finito) de fórmulas indexadas
bigOr :: [Int] -> (Int -> L) -> L
bigOr is f = undefined

-- Variable proposicional doblemente indexada
v2 :: Var -> Nat -> Nat -> L
v2 p i j = undefined


----------------------------------------------------------------------------------
-- Algunas funciones auxiliares 
----------------------------------------------------------------------------------

-- Pre: recibe un nombre de variable y dos entero positivos n y m.
-- Pos: genera todas las posibles permutaciones de declaraciones de variables
--      proposicionales con nombre p doblemente indexadas en el rango 1..n y 1..m 
--      en el formato SMT-LIB.
--      Por ejemplo, si n=4 y m=2 tenemos 4*2=8 declaraciones de variables.
-- RECOMENDACIÓN: para imprimir en consola ejecutar (por ej. si p="p", n=4 y m=2):  
--      putStrLn (genVars "p" 4 2)    
genVars :: String -> Nat -> Nat -> String
genVars p n m = foldr (\v b -> ("(declare-fun " ++ (show v) ++ " () Bool)\n") ++ b) "" vars
  where 
    vars = [V (p ++ (show i) ++ "_" ++ (show j)) | i <- [1..n], j <- [1..m]]

-- Pre: recibe una fórmula de LP.
-- Pos: pretty printing de la fórmula en formato SMT-LIB, esto es: parentizada y prefija.
toPrefix :: L -> String
toPrefix (V p)       = p
toPrefix (Neg a)     = "(not " ++ toPrefix a ++ ")"
toPrefix (Bin a And b) = "(and " ++ toPrefix a ++ " " ++ toPrefix b ++ ")"
toPrefix (Bin a Or  b) = "(or "  ++ toPrefix a ++ " " ++ toPrefix b ++ ")"
toPrefix (Bin a Imp b) = "(=> "  ++ toPrefix a ++ " " ++ toPrefix b ++ ")"
toPrefix (Bin a Iff b) = "(= "   ++ toPrefix a ++ " " ++ toPrefix b ++ ")"