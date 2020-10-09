-- Um player de músicas digitais precisa ranquear as músicas
-- de acordo com os seus nomes. O ranking de cada música con-
-- siste na soma dos valores de cada caractere alfabético do
-- nome de acordo com seus valores na tabela ASCII.
-- Por exemplo, “olhar 43” deve retornar
-- 111(o) + 108(l) + 104(h) + 97(a) + 114(r) = 534.
-- Desta forma, crie um programa em Haskell que dada uma lista
-- de strings (lista com nomes das músicas) retorne uma lista
-- com os rankings de cada uma das músicas descritas na lista
-- de entrada. OBS: Lembre-se que as funções isAlpha e ord já
-- estão disponíveis, onde a primeira retorna um booleano True
-- caso o caractere passado como parâmetro seja alfabético e
-- False caso contrario, enquanto que a segunda retorna o valor
-- ordinal de um caractere de acordo com a tabela ASCII.
import Data.Char (isAlpha, ord)

somaAlpha :: String -> Int
-- somaAlpha é uma função auxiliar, criada no intuito de receber
-- uma String (lista de caracteres) e recursivamente, somar cada
-- valor ordinal dos caracteres (ASCII)
somaAlpha [] = 0
somaAlpha (x : xs)
  -- se x é um caractere alfabético, então soma o valor ordinal de
  -- x com o próximo caractere alfabético encontrado
  | (isAlpha x) = (ord x) + (somaAlpha xs)
  | otherwise = (somaAlpha xs)

ranking :: [String] -> [Int]
ranking [] = [0] -- lista vazia
ranking [st] = [somaAlpha st] -- lista com apenas 1 elemento
-- recursão utilizando a função auxiliar para devolver o ranking da
-- lista de músicas
ranking (l : ls) = (somaAlpha l) : (ranking ls)