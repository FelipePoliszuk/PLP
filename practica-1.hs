-- Currificación y tipos

--Ejercicio 1

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

--Ejemplo de uso de curry
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

--I.
suma :: Num a => [a] -> a
suma = foldr (+) 0

elem2 :: Eq a => a -> [a] -> Bool
elem2 y = foldr (\x rec -> (x == y) || rec) False

masMas :: [a] -> [a] -> [a]
masMas xs ys = foldr(:) ys xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f = foldr (\x rec -> if f x 
                              then x : rec 
                              else rec) []

map2 :: (a -> b) -> [a] -> [b]
map2 f = foldr (\x rec -> f x : rec) []             

--II.
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x rec -> if f x rec then x else rec)

--III.
sumasParciales :: Num a => [a] -> [a]
sumasParciales []     = []
sumasParciales (x:xs) = x : map (+x) (sumasParciales xs)

--IV.
sumaAlt :: Num a => [a] -> a
sumaAlt = foldr (-) 0

--V.
sumaAltReverse :: Num a => [a] -> a
sumaAltReverse xs = snd (foldl f (1,0) xs)
  where
    -- acumulador = (signo, resultado)
    f (s,r) x = (-s, r + s*x)

-- Ejercicio 4    

-- I
-- permutaciones :: [a] -> [[a]]
-- permutaciones [] = [[]]
-- permutaciones xs = concatMap elegir [0 .. length xs - 1]
--   where
--     elegir i =
--       let (antes, x:despues) = splitAt i xs
--       in map (x:) (permutaciones (antes ++ despues))

-- permutaciones [1,2,3] -> [[1,2,3],[1,3,2],[2,3,1],[2,1,3],[3,1,2],[3,2,1]]

-- II
-- partes :: [a] -> [[a]]
-- partes xs = [drop 0 (take 0 xs)] ++ [drop 0 (take 1 xs)] ++ [drop 1 (take 2 xs)] ++ [drop 2 (take 3 xs)] ++ [drop 1 (take 3 xs)] ++ [drop 2 (take 1 xs)]
-- partes = 


-- partes [5, 1, 2] → [[], [5], [1], [2], [5, 1], [5, 2], [1, 2], [5, 1, 2]]
-- (en algún orden).



