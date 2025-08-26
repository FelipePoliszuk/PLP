
--Ejercicio 1

-- null, head, tail, init, last -> operaciones básicas sobre listas.

-- take, drop -> para cortar listas.

-- (++), concat -> para concatenar.

-- reverse -> invierte la lista.

-- elem -> pertenencia.

-- ...

-- null :: [a] -> Bool

-- head :: [a] -> a

-- tail :: [a] -> [a]

-- init :: [a] -> [a]

-- last :: [a] -> a

-- take :: Int -> [a] -> [a]

-- drop :: Int -> [a] -> [a]

-- reverse :: [a] -> [a]

-- elem :: Eq a => a -> [a] -> Bool


--Ejercicio 2

valorAbsoluto :: Float -> Float
valorAbsoluto x = if x < 0 then -x else x

bisiesto :: Int -> Bool
bisiesto x | mod x 4 == 0 && mod x 100 /= 0 = True
           | mod x 400 == 0 && mod x 100 == 0 = True
           | otherwise = False
             
factorial :: Int -> Int
factorial n | n < 0     = error "no definido para negativos"
            | n == 0    = 1
            | otherwise = n * factorial (n-1)

cantDivisores :: Int -> Int -> Int
cantDivisores n 0 = 0 
cantDivisores n m | mod n m == 0 = cantDivisores n (m-1) + 1
                  | otherwise = cantDivisores n (m-1)

esPrimo :: Int -> Bool
esPrimo n | cantDivisores n n == 2 = True
          | otherwise = False

cantDivisoresPrimosAux :: Int -> Int -> Int
cantDivisoresPrimosAux n 0 = 0
cantDivisoresPrimosAux n m | mod n m == 0 && esPrimo(m) = cantDivisoresPrimosAux n (m-1) +1
                           | otherwise = cantDivisoresPrimosAux n (m-1)

cantDivisoresPrimos :: Int -> Int       
cantDivisoresPrimos n =  cantDivisoresPrimosAux n n                   


--Ejercicio 3

-- data Maybe a = Nothing | Just a
-- data Either a b = Left a | Right b

inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1/x)

aEntero :: Either Int Bool -> Int
aEntero (Left n)      = n
aEntero (Right True)  = 1
aEntero (Right False) = 0


--Ejercicio 4

limpiar :: String -> String -> String
limpiar [] ys = ys
limpiar (x:xs) ys =  limpiar xs (limpiarAux x ys)


limpiarAux :: Char -> String -> String
limpiarAux c [] = []
limpiarAux c (x:xs) | c == x = limpiarAux c xs
                    | otherwise = x : limpiarAux c xs

--mas facil
limpiar2 :: String -> String -> String
limpiar2 xs ys = filter (`notElem` xs) ys

difPromedio :: [Float] -> [Float]
difPromedio xs = difPromedioAux xs (promedio xs)
  where
    difPromedioAux [] _ = []
    difPromedioAux (x:xs) prom = [x - prom] ++ difPromedioAux xs prom

promedio :: [Float] -> Float
promedio [] = 0
promedio (xs) = sum(xs) / fromIntegral (length xs)

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [x] = True
todosIguales (x:y:ys) | x /= y = False
                      | otherwise = todosIguales (y:ys)


-- Ejercicio 5

data AB a = Nil | Bin (AB a) a (AB a)
  deriving Show

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB _   = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin izq v der) = Bin (negacionAB izq) (not v) (negacionAB der)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin izq v der) = productoAB izq * v * productoAB der
