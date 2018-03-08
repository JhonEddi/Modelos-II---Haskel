{-
	modulo para trabajar con listas
-}

module Listas where
doblarLista::[Int]
doblarLista = [2*x | x <- [0..10]]

doblarLista2::[Int]->[Int]
doblarLista2 (x:xs) = [2*x | x <- (x:xs)]

sumaLista::[Int]->Int
sumaLista [] = 0
sumaLista(x:xs) = x + sumaLista xs

ultimo_elemento::[Int]->Int
ultimo_elemento [x] = x
ultimo_elemento (x:xs) = ultimo_elemento xs


mayorElemento :: [Int] -> Int
mayorElemento [a] = a
mayorElemento (x:xs) = if (x > mayorElemento xs) then x else mayorElemento xs

contarImpares :: [Int] -> Int
contarImpares [] = 0
contarImpares arreglo = length[n | n <- arreglo, odd n]