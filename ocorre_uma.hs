-- Escreva a funcao ocorreUma :: [Int] -> [Int]  em Haskell,
-- que deve retornar uma lista com os números inteiros que
-- aparecem apenas uma vez na lista passada como parâmetro.
-- Ex: ocorreUma [4,1,5,4,3,5]  deve retornar [1,3] .

ocorreUma :: [Int] -> [Int]
ocorreUma [] = [] -- Caso base para listas vazias ou caso
-- não existam números sem repetição em l
ocorreUma (l : ls)
  | elem l (ocorreUma ls) = [n | n <- (ocorreUma ls), n /= l]
  | otherwise = l : (ocorreUma ls)

-- Utilizando a função elem de Haskell, eu comparo cada ele-
-- mento de l, recursivamente (e utilizando compreensão de
-- lista). Caso haja elementos distintos em l, ocorreUma de-
-- volve estes elementos, caso contrário, devolve '[]'.
