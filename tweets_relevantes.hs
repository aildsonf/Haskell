-- Uma empresa deseja fazer uma análise sobre posts de personalidades
-- no Twitter para saber os temas mais abordados. Assim, crie uma função
-- em Haskell que recebe uma str de tweets (str de Strings), e devolve
-- uma str de tuplas, nas quais o primeiro elemento é a palavra e o
-- segundo a quantidade de vezes que esta palavra se repete em todos os
-- tweets. Faça um tratamento nos tweets para remover palavras irrelevantes
-- (artigos, preposições, conjunções) e assuma que dentro de cada String as
-- palavras estão separadas por espaço em branco. Na sua resposta cada palavra
-- deve aparecer em somente uma tupla. OBS: a função splitOn divide uma String
-- em uma str de Strings separadas pelo caracter passado como parâmetro.

-- Exemplo:
-- splitOn " " "foo bar baz glurk" -> ["foo","bar","baz","glurk"]

import Data.Char (toLower)
import Data.List (group)
import GHC.OldList (sort)

-- Função principal, palavrasRelevantes ["String1", "String2", ...]
palavrasRelevantes :: [String] -> [(String,Int)]
palavrasRelevantes [] = []
palavrasRelevantes [tweet] = contaPalavras tweet
palavrasRelevantes tweets = contaPalavras (uneTweets tweets)

uneTweets :: [String] -> String
-- Como a lista de Strings pode possuir vários tweets, ou seja, ["tweet1", "tweet2"],
-- essa função, serve para unir cada tweet em 1 única String, pois sem isso, a con-
-- tagem de palavras seria incorreta.
--      Ex: Sem essa função, ["a a a", "a a b"], contaPalavras devolveria [("a",3),
--      ("a",2), ("b",1)]. Com ela, o resultado é [("a", 5),("b", 1)].
uneTweets [] = []
uneTweets str = [ c | c <- (map toLower(unwords str)), not (c `elem` ",.?!-:;1234567890")]
-- unwords une 2 Strings em 1 só
-- map toLower(unwords str) coloca toda a String em lower case
-- a compreensão de lista remove alguns sinais de pontuação e números

contaPalavras :: String -> [(String, Int)]
contaPalavras str = map (\str -> (head str, length str)) $ group $ sort $ filter (\x -> x `notElem` ["o","a","os","as","um","uns","uma","umas","do","no","ao","pelo","dos","nos","aos","pelos","da","na","à","pela","das","nas","às","pelas","dum","num","duns","nuns","duma","numa","dumas","numas","ante","após","até","com","contra","de","desde","em","entre","para","perante","por","sem","sob","sobre","trás","e","nem","mas também","mas","porém","contudo","todavia","entretanto","ou","ora","já","quer","logo","pois","portanto","porque","que","se","como","caso","desde","contanto","tal","tanto","conforme","segundo","antes"]) $ words str

-- contaPalavras pode parecer confuso, já que muitas funções chamam outras, mas
-- a ideia é contar todas as palavras presentes em uma String, através de:

-- 1) words converte uma String numa lista de Strings, ex:
--      words "string de teste", devolve ["string","de","teste"]

-- 2) filter, de modo trivial, vai filtrar elementos na lista devolvida por words, ex:
--      filter (\x -> x `notElem` ["teste"]) ["String","de","teste"], devolve
--      ["String","de"]
--  OBS: a única forma que encontrei de filtrar os artigos, preposições, conjunções,
--  foi explicitando os mesmos no filtro (e coloquei apenas alguns pra fins de teste)

-- 3) sort, ordena a lista;

-- 4) group, agrupa elementos iguais em outras listas, ex:
--      group ["aa","aa","aa","ccc"], devolve [["aa","aa","aa"],["ccc"]]

-- 5) Por fim, map vai organizar o resultado de toda essa execução em uma lista de
-- tuplas, sob a seguinte formatação (palavra, qtdRepetições), ex:
--      map (\str -> (head str, length str)) [["aa","aa","aa"],["ccc"]], devolve
--      [("aa",3),("ccc",1)]

-- OBS: 
--  contaPalavras str = map (\str -> (head str, length str)) $ group $ sort $ filter (\x -> x `notElem` ["filtro"]) $ words str
-- é o mesmo que:
--  contaPalavras str = map (\str -> (head str, length str)) (group (sort (filter (\x -> x `notElem` ["filtro"]) (words str))))