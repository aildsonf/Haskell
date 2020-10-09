-- Suponha que precisamos calcular 2 elevado 
-- a um número n. Se n é par, por exemplo 2*m,
-- então: 2^n = 2^(2*m) = (2^m)^2

-- Se n é ímpar , por exemplo 2*m+1, então:
-- 2^n = 2^(2*m+1) = ((2^m)^2)*2

-- Desta forma, crie uma função recursiva
-- "eleva2:: Int -> Int" que computa 2 elevado
-- a um número n usando estas ideias.



eleva2 :: Int -> Int
eleva2 n
  | n == 0 = 1
  | n == 1 = 2
  | even n = 2 ^ (eleva2 (div n 2))
  | odd n = 2 * (eleva2 (n - 1))
  | otherwise = -1