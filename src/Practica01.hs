module Practica01 where

--TIPOS ALGEBRAICOS

--Ejercicio 1
data Shape = Circle Float | --representa el radio
            Square Float | --representa un lado
            Rectangle Float Float| --representa base y altura
            Triangle Float | --representa un lado
            Trapeze Float Float Float --representa base mayor, base menor y altura
            deriving (Show)

--Funcion que calcula el area de las figuras
area :: Shape -> Float
area (Circle r) = pi * r^2
area (Square l) = l * l
area (Rectangle b a) = b * a
area (Triangle b) = (b * b) / 2
area (Trapeze a b h) = ((a * b) * h) / 2

--Funcion que calcula el perimetro de las figuras
perimeter :: Shape -> Float
perimeter (Circle r) = (2 * r) * pi
perimeter(Square l) = l * 4
perimeter (Rectangle b a) = (2 * b) + (2 * a)
perimeter(Triangle b) = b * 3
perimeter (Trapeze a b h) = 2 * (sqrt (h^2 + ((a - b) / 2)^2)) + a + b


--Ejercicio 2 (Les toca arreglar el sinonimo)
type Point = (Float,Float)

-- Funcion para calcular la distancia entre dos puntos
distance :: Point -> Point -> Float
distance (x,y) (w,z) = (sqrt((w-x)^2+(z-y)^2))

--Funcion para calcular la distancia de un punto al origen
from0 :: Point -> Float
from0 (x,y) = sqrt ((x)^2 + (y)^2)

--Ejercicio 3
data Haskellium = Haskellium {name :: String,
                        lastName1 :: String,
                        lastName2 :: String,
                        location :: Point,
                        houseShape :: Shape} deriving Show

--Funcion para regresar el hijo de dos Haskelliums dado su nombre
son :: Haskellium -> Haskellium -> String -> Haskellium
son papa1 papa2 nino =Haskellium {
        name = nino,
        lastName1 = lastName1 papa1,
        lastName2 = lastName1 papa2,
        location = location papa1,
        houseShape = houseShape papa1
    }

--Funcion para calcular las unidades para construir la casa de un Haskellium
houseCost :: Haskellium -> Float
houseCost hask = (perimeter (houseShape hask) * 2.5) + area (houseShape hask)

--Funcion para calcular el tiempo que le toma a un Haskellium para llegar a su trabajo
timeToWork :: Haskellium -> Float
timeToWork hask =
    if from0 (location hask) < 300
    then from0 (location hask) / 30
    else from0 (location hask) / 70

--LISTAS Y FUNCIONES
--Ejercicio 1
palindromo :: String -> Bool
palindromo "" = True
palindromo [x] = True
palindromo 





--Ejercicio 2
miFoldr :: (a -> b -> b) -> b -> [a] -> b
miFoldr f z [] = z
miFoldr f z (x:xs) = f x (mifoldr f z xs)

--Ejercicio 3
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia [] = [[]]
conjuntoPotencia [x] = [[], [x]]
conjuntoPotencia (x:xs) =  

--ARBOLES

--Implementacion

data OneTwoTree a = Undefinedd

--Ejercicio 2
suma :: OneTwoTree Int -> Int
suma = undefined
