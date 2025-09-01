import Data.List (nub)

-- Currificación y tipos

-- Ejercicio 1

-- I. Tipo de cada función
-- max2 :: (Float, Float) -> Float
-- normaVectorial :: (Float, Float) -> Float
-- subtract :: Float -> Float -> Float
-- predecesor :: Float -> Float
-- evaluarEnCero :: (Float -> a) -> a
-- dosVeces :: (a -> a) -> a -> a
-- flipAll :: [(a -> b -> c)] -> [b -> a -> c]
-- flipRaro :: (a -> b -> c -> d) -> b -> a -> c -> d

-- II. Cuáles no están curriﬁcadas?

-- max2 y normaVectorial no están curriﬁcadas

-- Porque reciben una tupla como argumento.
-- En Haskell, una función curriﬁcada recibe los parámetros de a uno, no empaquetados en una tupla.

-- Ejercicio 2

-- I.

-- curry :: ((a, b) -> c) -> a -> b -> c
-- curry f x y = f (x, y)

-- Ejemplo de uso de curry
sumaTupla :: (Int, Int) -> Int
sumaTupla (x, y) = x + y

sumaCurri :: Int -> Int -> Int
sumaCurri = curry sumaTupla

-- II.

-- uncurry :: (a -> b -> c) -> ((a, b) -> c)
-- uncurry f (x, y) = f x y

-- III.

-- Se podría definir una función curryN, que tome una función de un número arbitrario de argumentos y
-- devuelva su versión currifcada?
-- Sugerencia: pensar cuál sería el tipo de la función.

-- Creo que no, porque no sabría los tipos de los n argumentos.

-- Ejercicio 3

-- I.
suma :: (Num a) => [a] -> a
suma = foldr (+) 0

elem2 :: (Eq a) => a -> [a] -> Bool
elem2 y = foldr (\x rec -> (x == y) || rec) False

masMas :: [a] -> [a] -> [a]
masMas xs ys = foldr (:) ys xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f =
  foldr
    ( \x rec ->
        if f x
          then x : rec
          else rec
    )
    []

map2 :: (a -> b) -> [a] -> [b]
map2 f = foldr (\x rec -> f x : rec) []

-- II.
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x rec -> if f x rec then x else rec)

-- III.
sumasParciales :: (Num a) => [a] -> [a]
sumasParciales [] = []
sumasParciales (x : xs) = x : map (+ x) (sumasParciales xs)

-- IV.
sumaAlt :: (Num a) => [a] -> a
sumaAlt = foldr (-) 0

-- V.
sumaAltReverse :: (Num a) => [a] -> a
sumaAltReverse xs = snd (foldl f (1, 0) xs)
  where
    -- acumulador = (signo, resultado)
    f (s, r) x = (-s, r + s * x)

-- Ejercicio 4

-- I (ver)
permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones xs = concatMap (\i -> map ((head (drop i xs)) :) (permutaciones (take i xs ++ drop (i + 1) xs))) [0 .. length xs - 1]

-- permutaciones [1,2,3] -> [[1,2,3],[1,3,2],[2,3,1],[2,1,3],[3,1,2],[3,2,1]]

-- II (ver)
partes :: [a] -> [[a]]
partes [] = [[]]
partes (x : xs) = partes xs ++ map (x :) (partes xs)

-- partes [5, 1, 2] → [[], [5], [1], [2], [5, 1], [5, 2], [1, 2], [5, 1, 2]]
-- (en algún orden).

-- III
prefijos :: [a] -> [[a]]
prefijos [] = [[]]
prefijos xs = prefijos (init xs) ++ [xs]

-- prefijo [5, 1, 2] → [[], [5], [5, 1], [5, 1, 2]]

-- extra --
sufijo :: [a] -> [[a]]
sufijo [] = [[]]
sufijo xs = xs : sufijo (tail xs)

-- IV (ver)

sublistas :: (Eq a) => [a] -> [[a]]
sublistas [] = [[]]
sublistas xs = nub (concatMap prefijos (suffixes xs))

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes (x : xs) = (x : xs) : suffixes xs

-- Ejercicio 5
elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x : xs) =
  if null xs
    then [x]
    else x : elementosEnPosicionesPares (tail xs)

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x : xs) = \ys ->
  if null ys
    then x : entrelazar xs []
    else x : head ys : entrelazar xs (tail ys)

-- ninguna es recursion estructural pues en el paso recursivo usan la cola xs sin ser la expresion (f xs)

-- Ejercicio 6















