todos :: [Bool] -> Bool
todos [] = True
todos (x:xs) = (x==True) && todos xs

paratodo :: [a] -> (a -> Bool) -> Bool
paratodo [] f = True
paratodo (x:xs) f = f x && paratodo xs f

existe :: [a] -> (a -> Bool) -> Bool
existe [] f = False
existe (x:xs) f = f x || existe xs f

sumatoria :: [a] -> (a -> Int) -> Int
sumatoria [] f = 0
sumatoria (x:xs) f = f x + sumatoria xs f
 
--ejercicio 14 practico 1

cuantGen :: (b -> b -> b) -> b -> [a] -> (a -> b) -> b
cuantGen b z [] t = z
cuantGen b z (x:xs) t = t x && cuantGen b z xs t 
