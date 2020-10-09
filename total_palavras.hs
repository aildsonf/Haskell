-- Crie um programa em Haskell que pergunta ao usuário se
-- ele deseja saber o total de palavras ou de caracteres
-- de um arquivo de texto. Em seguida ele deve solicitar
-- que o usuário forneça como entrada o caminho para um
-- arquivo de texto. Por último, de acordo com a primeira
-- escolha do usuário, ele deve contar palavras ou
-- caracteres e informar na saída padrão o total de elementos
-- de acordo com a escolha. Caso a escolha seja caracter,
-- espaços em branco, caracteres de tabulação e nova linha
-- não devem ser considerados na contabilização. Abaixo segue
-- um exemplo de como o prompt do seu programa deve funcionar:

-- "Digite 1 para contar palavras ou 2 para contar caracteres:"
-- -> 2

-- "Digite o caminho do arquivo de texto:"
-- -> exemplo.txt

-- "O total de caracteres no arquivo exemplo.txt é 156."

import Text.Printf (printf)

totalPalavras :: IO ()
totalPalavras = do
  putStrLn "Digite 1 para contar palavras ou 2 para contar caracteres: "
  escolha <- getChar
  putStrLn "Digite o caminho do arquivo de texto: "
  caminho <- getLine
  contagem escolha caminho

-- Função auxiliar para contar os caracteres ou palavras
-- Essa função utiliza algumas notações presentes na mani-
-- pulação de monóides em Haskell (>>, >>=), disponível em:
-- https://wiki.haskell.org/Monad
contagem :: Char -> String -> IO ()
contagem escolha caminho
  | (escolha == '1') = printf "O total de palavras no arquivo %s é: " caminho >> readFile caminho >>= print . length . words
  | (escolha == '2') = printf "O total de caracteres no arquivo %s é: " caminho >> readFile caminho >>= print . length . filter (/= ' ')
