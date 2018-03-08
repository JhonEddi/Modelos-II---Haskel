invertir::[Int]->[Int]
invertir [] = []
invertir (x:xs) = (invertir xs) ++ [x]

cantidadDigitos::Int->Int
cantidadDigitos n = if n<=9 then 1 else 1 + (cantidadDigitos (div n 10))

potencia::Int->Int->Int
potencia a b = if (b==0) then 1 else if (b>=1) then a * potencia a b-1