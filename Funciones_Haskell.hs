module Funciones where

sumar :: [Int]->Int
sumar []=0
sumar (x:xs)=x+sumar(xs)

producto :: Int->Int->Int
producto a 0 = 0
producto a 1 = a
producto a b = a + (producto a (b-1))

division :: Int->Int->Int
division 0 b = 0
division a b = 1 + division (a-b) b

fibonacci :: Int->Int
fibonacci a = if a <= 2 then 1 else (fibonacci(a-1) + fibonacci(a-2))