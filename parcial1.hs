-- TEMA C
{-

-- Ejercicio 1
data Deporte = Futbol | Basket | Tenis | Valorant | Dota2
type MinJugadores = Int 

minimaCantidad :: Deporte -> MinJugadores
minimaCantidad Futbol = 1 
minimaCantidad Basket = 1
minimaCantidad Tenis = 2
minimaCantidad Valorant = 1
minimaCantidad Dota2 = 5

-- ejemplo de ejecución: minimaCantidad Futbol
-}

-- Ejercicio 2
data Deporte = Futbol | Basket | Tenis | Valorant | Dota2 deriving (Show, Eq)
type MinJugadores = Int 
type NombrePersona = String
data PracticoDeporte = Ninguna | AgregaDeporte Deporte NombrePersona PracticoDeporte deriving (Show, Eq)

deporte :: PracticoDeporte -> Deporte -> NombrePersona -> Bool
deporte Ninguna _ _ = False
deporte (AgregaDeporte d n xs) d1 n1 | (d == d1) && (n == n1) = True
                                     | otherwise = deporte xs d1 n1

-- Ejemplo de ejecución: deporte (AgregaDeporte Futbol "Manuel" Ninguna) Futbol "Manuel"

{-

-- Ejercicio 3
data Deporte = Futbol | Basket | Tenis | Valorant | Dota2 deriving (Show, Eq)
type MinJugadores = Int 
type NombrePersona = String
data PracticoDeporte = Ninguna | AgregaDeporte Deporte NombrePersona PracticoDeporte deriving (Show, Eq)

type EquipoFavorito = String
data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b) deriving (Show)

agregaEquipoFavorito :: ListaAsoc Deporte EquipoFavorito -> Deporte -> EquipoFavorito -> ListaAsoc Deporte EquipoFavorito
agregaEquipoFavorito Vacia d ef = (Nodo d ef Vacia) 
agregaEquipoFavorito (Nodo d ef l1) d1 ef1 = Nodo d ef (agregaEquipoFavorito l1 d1 ef1)

-- Ejecución de la función: agregaEquipoFavorito (Nodo Futbol "Boca" Vacia) Basket "Instituto"

-}

-- Ejercicio 4 
data Arbol a = Hoja | Rama ( Arbol a ) a ( Arbol a ) deriving (Show, Ord, Eq)            

aCuantos :: Arbol Int -> Int -> Int
aCuantos Hoja _ = 0
aCuantos (Rama r1 x r2) y | (x<y) = 1 + aCuantos r1 y + aCuantos r2 y
                          | otherwise = aCuantos r1 y + aCuantos r2 y   


-- Ejecución de la función: aCuantos (Rama (Rama Hoja 10 Hoja) 15 (Rama Hoja 20 Hoja)) 30                           