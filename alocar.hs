-- Crie uma função em Haskell alocar::[Int]->Int->[[Int]] que recebe
-- uma lista de inteiros e um inteiro n e ela deve retornar uma lista
-- com listas de n elementos de forma que os elementos nas sublistas
-- são inseridos de forma consecutiva varrendo os elementos da lista
-- principal. Por exemplo: 

-- alocar [4,23,2,5,6,8,12,57] 3 = [[4,23,2],[5,6,8],[12,57]]

alocar :: [Int] -> Int -> [[Int]]
alocar [] _ = [] 
alocar [num] _ = [[num]]                           
alocar lista num = (l : alocar ls num)        
    where (l, ls) = (splitAt num lista)

-- A função 'splitAt' pega os n primeiros elementos de uma lista e devolve
-- uma tupla, onde ([n-primeiros elementos], [restante da lista]).
-- Chamando recursivamente pro restante da lista, 'alocar' devolve sub-listas
-- de tamanho n ou <= n (caso a quantidade de elementos restantes seja 1..n-1).