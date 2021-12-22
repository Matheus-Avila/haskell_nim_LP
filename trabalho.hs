import Data.List
import Data.Char
import Control.Arrow (Arrow(first))
import Data.Attoparsec (string)

import Data.Array.MArray
import Data.Array.IO





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
    let palitosStr = ["1", "3", restStr, "7"] in do
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

data SomeData = SomeData { array :: IOArray Int Int }

menu = do
  arr <- newArray (1,4) 1
  let d = SomeData { array = arr }
  writeArray (array d) 2 3
  writeArray (array d) 3 5
  writeArray (array d) 4 7
  a <- readArray (array d) 3
  b <- readArray (array d) 4
  print (a,b)

-- data SomeData = SomeData { array :: IOArray Int Float }

-- amenu = do 
--     arr <- newArray (1,) 1
--     a <- readArray arr 1
--     writeArray arr 1 64
--     b <- readArray arr 1 
--     print (a,b)