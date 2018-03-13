{-
	modulo de funciones recursivas
-}

module EjerciciosRecursividad where
{- 1 -}
producto :: Int->Int->Int
producto a 0 = 0
producto a 1 = a
producto a b = a + (producto a (b-1))

{- 2 -}
division :: Int->Int->Int
division 0 b = 0
division a b = 1 + division (a-b) b

{- 3 -}
fibonacci :: Int->Int
fibonacci a = if a <= 2 then 1 else (fibonacci(a-1) + fibonacci(a-2))

{- 4 -}
potencia :: Int->Int->Int
potencia a 0 = 1  
potencia a b = a*(potencia a (b-1))

{- 5 -}
sumaDigitos :: Int->Int
sumaDigitos a 
	| a < 10 = a 
	| otherwise = (mod a 10) + (sumaDigitos (div a 10))

{- 6 -}
mayorDigito :: Int->Int
mayorDigito a
	| a < 10 = a
	|(mod a 10) > mayorDigito(div a 10) = (mod a 10)
	| otherwise = mayorDigito(div a 10) 

{- 9 -}
cantidadDigitos :: Int->Int
cantidadDigitos n 
	| n < 10 = 1 
	| otherwise = 1 + (cantidadDigitos (div n 10))
	
{- 7 -}
invertir :: Int -> Int
invertir n
	| n <10 = n
	| otherwise = (10^((cantidadDigitos n)-1))*(mod n 10) + invertir (div n 10)
	
{- 8 -}	
palindromo :: Int -> [Char]
palindromo n
	| cantidadDigitos n <= 1 =  "Palindromo"
    | (mod n 10) == (mod (invertir n) 10) = palindromo(div (n - ((10^(cantidadDigitos(n)-1))* (div n (10^((cantidadDigitos n)-1))))) 10)
    | otherwise = "No palindromo"