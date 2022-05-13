-- Ejercicio 1a
esCero ::  Int -> Bool
esCero x = (x==0)

-- Ejercicio 1b 
esPositivo :: Int -> Bool 
esPositivo x = x>0

-- Ejercicio 1b'
esPositivo' :: Float -> Bool
esPositivo' x = x>0

-- Ejercicio 1c (vocales sin tilde)
esVocal :: Char -> Bool
esVocal x | (x=='a' || x=='e' || x=='i' || x=='o' || x=='u') = True
          | otherwise = False

-- Ejercicio 2a
paratodo :: [Bool] -> Bool
paratodo [] = True
paratodo (x:xs) = x && paratodo xs 

-- Ejercicio 2b
sumatoria :: [Int] -> Int
sumatoria [] = 0 
sumatoria (x:xs) = x + sumatoria xs

-- Ejercicio 2c
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs

-- Ejercicio 2d {x ∈ N, con N = Naturales}
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

-- Ejercicio 2e 
promedio :: [Int] -> Int
promedio [] = 0
promedio (x:xs) = div (sumatoria (x:xs)) (length (x:xs))  

-- Ejercicio 3
pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece n (x:xs) = (n == x) || pertenece n xs

-- Ejercicio 4a 
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] _ = True
paratodo' (x:xs) f = (f x) && paratodo' xs f

-- Ejercicio 4b 
existe' :: [a] -> (a -> Bool) -> Bool 
existe' [] _ = False
existe' (x:xs) f = (f x) || existe' xs f

-- Ejercicio 4c
sumatoria' :: Num a => [a] -> (a -> Int) -> Int
sumatoria' [] _ = 0
sumatoria' (x:xs) f = (f x) + (sumatoria' xs f)

-- Ejercicio 4d 
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] _ = 1
productoria' (x:xs) f = (f x) * productoria' xs f

-- Ejercicio 5 
paratodo'' :: [Bool] -> Bool
paratodo'' xs = paratodo' xs id

-- Ejercicio 6a
todosPares :: [Int] -> Bool 
todosPares xs = paratodo' xs even

-- Ejercicio 6b
esMultiplo :: Int -> Int -> Bool
esMultiplo n x = mod n x == 0 || mod x n == 0

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n xs = existe' xs (esMultiplo n)

-- Ejercicio 6c
sumaCuadrados :: Int -> Int
sumaCuadrados x = sumatoria' [0..x] (^2)

-- Ejercicio 6d
factorial' :: Int -> Int
factorial' x = productoria' [1..x] id

-- Ejercicio 6e 
esPar :: Int -> Int  
esPar x | even x = x
        | otherwise = 1 

multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria' xs esPar 

-- Ejercicio 7
-- map aplica una función a cada elemento de una lista y devuelve otra lista.
-- filter aplicado a un predicado y una lista, devuelve una lista con los elementos que satisfacen el predicado.
-- succ es equivalente a: map (+1) [1, -4, 6, 2, -8] = [2, -3, 7, 3, -7]
-- filter esPositivo equivale a: filter (>=0) [1, 2, (-1), 0] = [1, 2, 0]

-- Ejercicio 8a
duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = (x*2) : duplica xs

-- Ejercicio 8b
duplica' :: [Int] -> [Int] 
duplica' xs = map (*2) xs

-- Ejercicio 9a
soloPares :: [Int] -> [Int] 
soloPares [] = []
soloPares (x:xs) | even x = x : soloPares xs
                 | otherwise = soloPares xs

-- Ejercicio 9b
soloPares' :: [Int] -> [Int]
soloPares' xs = filter even xs 

-- Ejercicio 9c
multiplicaPares' :: [Int] -> Int
multiplicaPares' xs = productoria (filter even xs)

-- Ejercicio 10a
primIgualesA :: Eq a => a -> [a] -> [a]
primIgualesA _ [] = []
primIgualesA y (x:xs) | x == y = x : primIgualesA y xs
                      | otherwise = []

-- Ejercicio 10b
primIgualesA' :: Eq a => a -> [a] -> [a]
primIgualesA' y xs = takeWhile (==y) xs

-- Ejercicio 11a 
primIguales :: Eq a => [a] -> [a]
primIguales [] = []
primIguales [x] = [x]
primIguales (x:xs) | head (x:xs) == head (tail (x:xs)) = x : primIguales xs
                   | otherwise = [x]

-- Ejercicio 11b 
primIguales' :: Eq a => [a] -> [a]
primIguales' xs = primIgualesA' (head xs) xs

-- Ejercicio 12
-- a) f :: (a, b) -> ...
--    f x = ...
-- Está bien tipado, el primer argumento es una tupla de dos elementos y cumple para todos los casos. 

-- b) f :: (a, b) -> ...
--    f (x , y) = ...
-- Está bien tipado, es una tupla de dos elementos y cumple para todos los casos. 

-- c) f :: [(a, b)] -> ...
--    f (a , b) = ...
-- Está mal tipado, el argumento tiene que ser una lista de tuplas con dos elementos.

-- d) f :: [(a, b)] -> ...
--    f (x:xs) = ...
-- Está bien tipado, lista de tuplas, no contempla el caso donde la lista es vacía.

-- e) f :: [(a, b)] -> ...
--    f ((x, y) : ((a, b) : xs)) = ...
-- Está bien tipado, lista de tuplas de dos elementos, no toma el caso de lista vacía, tampoco cuando la lista tiene un solo elemento: [x] 

-- f) f :: [(Int, a)] -> ...
--    f [(0, a)] = ...
-- Está bien tipado, lista de tuplas de un componente Int y una variable de tipo, la función solo toma el 0 en primer componente, no toma el caso de lista vacía ni lista con más tuplas.

-- g) f :: [(Int, a)] -> ...
--    f ((x, 1) : xs) = ...
-- Está bien tipado, lista de tuplas, f no cubre todos los casos ya que fija el segundo elemento de la tupla. No cubre la lista vacía.

-- h) f :: [(Int, a)] -> ...
--    f ((1, x) : xs) = ...
-- Está bien tipado, lista de tuplas, f no cubre todos los casos ya que fija el primer elemento de la tupla. No cubre la lista vacía.

-- i) f :: (Int -> Int) -> Int -> ...
--    f a b = ...
-- Está bien tipado, el primer argumento es una función que toma y devuelve Int, el segundo es un Int. Cubre todos los casos.

-- j) f :: (Int -> Int) -> Int -> ...
--    f a 3 = ...
-- Está bien tipado, el primer argumento es una función que toma y devuelve Int, el segundo es un Int. Restringido a 3 en el segundo argumento.

-- k) f :: (Int -> Int) -> Int -> ...
--    f 0 1 2 = ...
-- Está mal tipado, la función segun la definción deberia tener dos argumentos. 

-- l) f :: a -> (a -> a) -> ...
--    f a g = ...
-- Está bien tipado, primer argumento es una variable de tipo y el segundo una función. Cubre todos los casos.


-- Ejercicio 13
-- a) 
-- f1 :: (a, b) -> b
-- f1 (x, y) = y

-- b)
-- f2 :: (a, b) -> c
-- No se puede debido a que no puedo aplicar nada a la tupla y que de como resultado c sin limitar el tipo de la tupla.

-- c) 
-- f3 :: a -> b
-- f3 x = ...
-- No se puede, ambos deberian ser del mismo tipo.

-- d)
-- f4 :: (a -> b) -> a -> b
-- f4 g x = g x 

-- e) 
-- f5 :: (a -> b) -> [a] -> [b]
-- f5 h [] = []
-- f5 h (x:xs) = (h x):(f5 h xs)

-- f ) 
-- f6 :: (a -> b) -> a -> c
-- No se puede ya que el tipo que retorna la función deberia ser b.

-- g) 
-- f7 :: (a -> b) -> (b -> c) -> a -> c
-- f7 j k x = k (j x)