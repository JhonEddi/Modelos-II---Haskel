{-
	modulo de funciones con listas
-}

module FuncionesListas where

--invertir una lista recursivamente
invertirLista ::[Int]->[Int]
invertirLista [] = []
invertirLista (x:xs) = (invertirLista xs) ++ [x]

--contar numeros impares de una lista recursivamente
contarImpares::[Int]->Int
contarImpares[] = 0
contarImpares(x:xs)
 | mod x 2 == 1 = 1 + contarImpares(xs)
 | otherwise = contarImpares(xs)

--contar numeros impares con funciones de Haskell
contarImparFunciones :: [Int]->Int
contarImparFunciones(x:xs) = length (filter odd (x:xs))

--encontrar mayor de una lista recursivamente
mayorElemento :: [Int] -> Int
mayorElemento [a] = a
mayorElemento (x:xs) 
 | x > mayorElemento xs = x 
 | otherwise = mayorElemento xs
