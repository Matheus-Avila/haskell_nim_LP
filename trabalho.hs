import Data.List
import Data.Char
import Control.Arrow (Arrow(first))
palitosStr = ["1", "3", "5", "7"]
-- Converte a string para int e podemos operar com ela assim
-- *Main> let i = read k :: Integer
-- *Main> i
-- 1
main = do
    putStrLn "Escolha um modo de jogo"
    escolha <- getLine
    putStrLn ("0: " ++ palitosStr !! 0)
    putStrLn ("1: " ++ palitosStr !! 1)
    putStrLn ("2: " ++ palitosStr !! 2)
    putStrLn ("3: " ++ palitosStr !! 3)
    putStrLn ("Voce escolheu " ++ escolha)
