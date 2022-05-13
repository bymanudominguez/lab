-- Ejercicio 1a y 2a
data Carrera = Matematica | Fisica | Computacion | Astronomia 
  deriving (Eq, Ord, Show)

-- Ejercicio 1b
titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Ciencias de la Computacion"
titulo Astronomia = "Licenciatura en Astronomia"

-- Ejercicio 3a
type Ingreso = Int
data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar deriving (Eq, Ord, Show)
data Area = Administrativa | Ensenanza | Economica | Postgrado deriving (Eq, Ord, Show)
data Persona = Decane | Docente Cargo | NoDocente Area | Estudiante Carrera Ingreso deriving (Eq, Ord, Show) 

-- Ejercicio 3b
-- Docente :: Cargo -> Persona

-- Ejercicio 3c
mismocargo :: Cargo -> Cargo -> Bool
mismocargo Titular Titular = True
mismocargo Asociado Asociado = True
mismocargo Adjunto Adjunto = True
mismocargo Asistente Asistente = True
mismocargo Auxiliar Auxiliar = True
mismocargo _ _ = False 

cuantos_doc :: [Persona] -> Cargo -> Int 
cuantos_doc [] _ = 0
cuantos_doc ((Docente x):xs) c | mismocargo x c = 1 + cuantos_doc xs c
                               | otherwise = cuantos_doc xs c 
cuantos_doc (_:xs) c = cuantos_doc xs c 

-- Ejercicio 3d 
cuantos_doc' :: [Persona] -> Cargo -> Int 
cuantos_doc' xs c = length (filter (==Docente c) xs) 

-- Ejercicio 4a 
primerElemento :: [a] -> Maybe a 
primerElemento [] = Nothing
primerElemento xs = Just (head xs) 

-- Ejercicio 5a
data Cola = VaciaC | Encolada Persona Cola deriving (Show, Eq)

-- 1)
atender :: Cola -> Maybe Cola
atender (VaciaC) = Nothing
atender (Encolada _ x) = Just x

-- 2)
encolar :: Persona -> Cola -> Cola
encolar x VaciaC = Encolada x VaciaC
encolar x (Encolada y z) = Encolada y (encolar x z)

-- 3) 
busca :: Cola -> Cargo -> Maybe Persona 
busca VaciaC _ = Nothing
busca (Encolada persona cola) cargo = case persona of 
              Docente cargox | mismocargo cargo cargox -> Just persona
              _ -> busca cola cargo 

-- Ejercicio 5b 
-- El tipo cola se asemeja al tipo lista [] del prelude en Haskell

-- Ejercicio 6a
-- type Guía_telefónica = ListaAsoc String Int

-- Ejercicio 6b
data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b) deriving (Show)

-- 1) 
la_long :: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo _ _ Vacia) = 0
la_long (Nodo _ _ (Nodo a b l)) = 1 + la_long (Nodo a b l)

-- 2) 
la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia l = l
la_concat (Nodo a b l1) l2 = Nodo a b (la_concat l1 l2)

-- 3)
la_pares :: ListaAsoc a b -> [(a, b)]
la_pares Vacia = []
la_pares (Nodo a b l) = (a, b) : (la_pares l)

-- 4)

la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia _ = Nothing 
la_busca (Nodo a b l) c | (a == c) = Just b
                        | otherwise = la_busca l c

-- 5)
la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar _ Vacia = Vacia
la_borrar c (Nodo a b l) | (a == c) = la_borrar c l 
                         | otherwise = Nodo a b (la_borrar c l)

-- Ejercicio 7
data Arbol a = Hoja | Rama ( Arbol a ) a ( Arbol a ) deriving (Show, Ord, Eq)

type Prefijos = Arbol String
can , cana , canario , canas , cant , cantar , canto :: Prefijos
can = Rama cana "can" cant
cana = Rama canario "a" canas
canario = Rama Hoja "rio" Hoja
canas = Rama Hoja "s" Hoja
cant = Rama cantar "t" canto
cantar = Rama Hoja "ar" Hoja
canto = Rama Hoja "o" Hoja

-- Ejercicio 7a
a_long :: Arbol a -> Int 
a_long Hoja = 0
a_long (Rama r1 _ r2) = 1 + a_long r1 + a_long r2 

-- Ejercicio 7b
a_hojas :: Arbol a -> Int
a_hojas Hoja = 1
a_hojas (Rama r1 _ r2) = a_hojas r1 + a_hojas r2

-- Ejercicio 7c
a_inc :: Num a => Arbol a -> Arbol a
a_inc Hoja = Hoja
a_inc (Rama r1 x r2) = (Rama (a_inc r1) (x+1) (a_inc r2))

-- Ejercicio 7d
a_map :: (a -> b) -> Arbol a -> Arbol b 
a_map _ Hoja = Hoja
a_map f (Rama r1 x r2) = (Rama (a_map f r1) (f x) (a_map f r2))
 
-- Ejercicio 7c' con a_map
a_inc' :: Num a => Arbol a -> Arbol a
a_inc' Hoja = Hoja
a_inc' (Rama r1 x r2) = (Rama (a_map (+1) r1) (x+1) (a_map (+1) r2))