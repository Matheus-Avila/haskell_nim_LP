import Data.List
import Data.Char
import Control.Arrow (Arrow(first))
import Data.Attoparsec (string)


-- Converte a string para int e podemos operar com ela assim
-- *Main> let i = read k :: Integer
-- *Main> i
-- 1
strToInt x = read x :: Int
intToStr x = show x :: String
interface = do
    let linha0Str = palitosStr !! 0
    let linha1Str = palitosStr !! 1
    let linha2Str = palitosStr !! 2
    let linha3Str = palitosStr !! 3
    putStrLn "Configuração dos palitos:"
    putStrLn ("0: " ++ linha0Str)
    putStrLn ("1: " ++ linha1Str)
    putStrLn ("2: " ++ linha2Str)
    putStrLn ("3: " ++ linha3Str)
    putStrLn "Escolha uma linha para remover os palitos:"
    linha <- getLine
    let linhaInt = strToInt linha
    let valStr = palitosStr !! linhaInt
    let numPalitosLinha = strToInt valStr
    putStrLn ("Escolha quantos palitos quer remover. O maximo eh :" ++ valStr)
    qtdePalitos <- getLine
    let qtdePalitosInt = strToInt qtdePalitos
    let rest = numPalitosLinha - qtdePalitosInt
    let restStr = intToStr rest
    putStrLn ("rest:"++restStr)
    let palitosStr = ["1", "3", restStr, "7"]
    putStrLn "Configuração inicial:"
    putStrLn ("0: " ++ head palitosStr)
    putStrLn ("1: " ++ palitosStr !! 1)
    putStrLn ("2: " ++ palitosStr !! 2)
    putStrLn ("3: " ++ palitosStr !! 3)
    if maximum palitosStr /= "0"
    then do
        interface
    else
        putStrLn "Acavou!!"

palitosStr = ["1", "3", "5", "7"]
main  = do {
    putStrLn "Escolha um modo de jogo";
    escolha <- getLine;
    putStrLn ("Voce escolheu " ++ escolha);
    if escolha == "1"
        then  do
    if maximum palitosStr /= "0"
    then do 
        interface;
        else do putStrLn ("Acabou!")
        else do
        putStrLn ("Acabou!");
}
-- main = do
--     putStrLn "Escolha um modo de jogo"
--     escolha <- getLine
--     putStrLn ("Voce escolheu " ++ escolha)
--     if escolha == "1"
--         then  do
--     putStrLn "Configuração inicial:"
--     putStrLn ("0: " ++ palitos !! 0)
--     putStrLn ("1: " ++ palitos !! 1)
--     putStrLn ("2: " ++ palitos !! 2)
--     putStrLn ("3: " ++ palitos !! 3)
--     putStrLn "Escolha uma linha para remover os palitos:"
--     linha <- getLine
--     putStrLn "Escolha quantos palitos quer remover:"
--     linha <- getLine
--     putStrLn "Escolha uma linha para remover os palitos:"
--         else do
--             putStrLn ("0: " ++ palitos !! 0)
--             putStrLn ("1: " ++ palitos !! 1)