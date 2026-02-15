module Practica01 where

--TIPOS ALGEBRAICOS

--Ejercicio 1
data Shape = Circle Float | --representa el radio
            Square Float | --representa un lado
            Rectangle Float Float| --representa base y altura
            Triangle Float | --representa un lado
            Trapeze Float Float Float --representa base mayor, base menor y altura
            deriving (Show, Eq)

--Funcion que calcula el area de las figuras
area :: Shape -> Float
area (Circle x) = pi * x**2 
area (Square x) = x * x
area (Rectangle x y) = x * y
area (Triangle x) = (sqrt(3)/4)*(x**2)
area (Trapeze x y z) = ((x+y)*z)/2

--Funcion que calcula el perimetro de las figuras
perimeter :: Shape -> Float
perimeter (Circle x) = 2*pi*x
perimeter (Square x) = 4*x
perimeter (Rectangle x y) = 2*x+2*y
perimeter (Triangle x) = 3*x
perimeter (Trapeze x y z) = x + y + 2*sqrt(z**2 + ((x-y)/2)**2)


--Ejercicio 2 (Les toca arreglar el sinonimo)
type Point = (Float, Float)

-- Funcion para calcular la distancia entre dos puntos
distance :: Point -> Point -> Float
distance (x_1,y_1) (x_2,y_2) = sqrt((x_2-x_1)**2 + (y_2-y_1)**2) 

--Funcion para calcular la distancia de un punto al origen
from0 :: Point -> Float
from0 (x,y) = sqrt(x**2 + y**2) 

--Ejercicio 3
data Haskellium = Haskellium {name :: String,
                              lastName1 :: String,
                              lastName2 :: String,
                              location :: Point,
                              houseShape :: Shape} deriving Show

--Funcion para regresar el hijo de dos Haskelliums dado su nombre
son :: Haskellium -> Haskellium -> String -> Haskellium
son haskellium1 haskellium2 nombre = (Haskellium {name = nombre, lastName1 = lastName1 haskellium1, lastName2 = lastName1 haskellium2, location = location haskellium1, houseShape = houseShape haskellium1})

--Funcion para calcular las unidades para construir la casa de un Haskellium
houseCost :: Haskellium -> Float
houseCost haskellium = (perimeter (houseShape haskellium)*2.5) + area (houseShape haskellium)

--Funcion para calcular el tiempo que le toma a un Haskellium para llegar a su trabajo
timeToWork :: Haskellium -> Float
timeToWork haskellium = if distancia < 300 then distancia/30 else distancia/70
                        where distancia = from0 (location haskellium)

--LISTAS Y FUNCIONES
--Ejercicio 1
palindromo :: String -> Bool
palindromo [] = True
palindromo xs = xs == (reversa xs)

--Funcion auxiliar que nos devuelve la palabra en reversa para el ejercicio palindromo.
reversa :: String -> String
reversa [] = []
reversa (x:xs) = reversa xs ++ [x] 

--Ejercicio 2
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b
myFoldr f b (x:xs) = f x (myFoldr f b xs)

--Ejercicio 3
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] =  [[]]
conjuntoPotencia (x:xs) = conjuntoPotencia xs ++ [x:y | y <- conjuntoPotencia xs]

--ARBOLES

--Implementacion

data OneTwoTree a = Void | Node a (OneTwoTree a) | Branch a (OneTwoTree a) (OneTwoTree a)

--Ejercicio 2
suma :: OneTwoTree Int -> Int
suma (Void) = 0
suma (Node a b) = a + (suma b)
suma (Branch a b c) = a + (suma b) + (suma c)