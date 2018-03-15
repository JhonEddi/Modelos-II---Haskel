{-
	modulo de funciones con listas
-}

module FuncionesListas where

--invertir una lista recursivamente
invertirLista ::[Int]->[Int]
invertirLista [] = []
invertirLista (x:xs) = (invertirLista xs) ++ [x]

--suma de los numeros pares de una lista recursivamente
sumaPares::[Int]->Int
sumaPares[] = 0
sumaPares(x:xs)
 | mod x 2 == 0 = x + sumaPares(xs)
 | otherwise = sumaPares(xs)
 
--suma de los numeros pares de una lista con funciones
sumaParesFunciones::[Int]->Int
sumaParesFunciones(x:xs) = foldr (+) 0 [x | x <- (x:xs), mod x 2 == 0]

--contar numeros impares de una lista recursivamente
contarImpares::[Int]->Int
contarImpares[] = 0
contarImpares(x:xs)
 | mod x 2 == 1 = 1 + contarImpares(xs)
 | otherwise = contarImpares(xs)

--contar numeros impares con funciones de Haskell
contarImparFunciones :: [Int]->Int
contarImparFunciones(x:xs) = length (filter odd (x:xs))

--Lista dentro de una lista recursivamente
--Elemento dentro de una lista
pertenece :: Int->[Int]-> Bool
pertenece x [] = False
pertenece x (y:ys)
 | x==y = True
 | otherwise =  pertenece x ys

perteneceLista :: [Int] -> [Int] -> Bool
perteneceLista [] [] = True
perteneceLista [] lista = True
perteneceLista lista [] = False
perteneceLista (x:xs) (y:ys) = pertenece x (y:ys) && perteneceLista xs (y:ys)

--Lista dentro de una lista con funciones
--Elemento dentro de una lista
en_lista :: [Int]->Int-> Bool
en_lista [] x = False
en_lista (y:ys) x
 | x==y = True
 | otherwise =  en_lista ys x
  
perteneceListaF :: [Int] -> [Int] -> Bool
perteneceListaF (x:xs) (y:ys) = foldl (&&) True (map (en_lista (x:xs)) (y:ys))

--encontrar mayor de una lista recursivamente
mayorElemento :: [Int] -> Int
mayorElemento [a] = a
mayorElemento (x:xs) 
 | x > mayorElemento xs = x 
 | otherwise = mayorElemento xs
