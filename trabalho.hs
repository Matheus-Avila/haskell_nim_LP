import Data.List
import Data.Char
import Control.Arrow (Arrow(first))
import Data.Attoparsec (string)

import Data.Array.MArray
import Data.Array.IO





strToInt x = read x :: Int
intToStr x = show x :: String
interface = do
    read <- readFile "palitos.txt"
    palitosArr <- rList read
    let linha0Int = palitosArr !! 0
    let linha1Int = palitosArr !! 1
    let linha2Int = palitosArr !! 2
    let linha3Int = palitosArr !! 3
    let linha0Str = intToStr linha0Int
    let linha1Str = intToStr linha1Int
    let linha2Str = intToStr linha2Int
    let linha3Str = intToStr linha3Int
    putStrLn "Configuração dos palitos:"
    putStrLn ("0: " ++ linha0Str)
    putStrLn ("1: " ++ linha1Str)
    putStrLn ("2: " ++ linha2Str)
    putStrLn ("3: " ++ linha3Str)
    putStrLn "Escolha uma linha para remover os palitos:"
    linha <- getLine
    let linhaInt = strToInt linha
    let numPalitosLinha = palitosArr !! linhaInt
    let valStr = intToStr numPalitosLinha
    putStrLn ("Escolha quantos palitos quer remover. O maximo eh :" ++ valStr)
    qtdePalitos <- getLine
    let qtdePalitosInt = strToInt qtdePalitos
    let rest = numPalitosLinha - qtdePalitosInt
    let restStr = intToStr rest
    putStrLn ("rest:"++restStr)
    if linhaInt == 0 
        then do 
            let linha0Int = rest
            let linha0Str = intToStr linha0Int
            let linha1Str = intToStr linha1Int
            let linha2Str = intToStr linha2Int
            let linha3Str = intToStr linha3Int
            y <- writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
            print rest
    else print "linha0" 
    if linhaInt == 1
        then do
            let linha1Int = rest
            let linha0Str = intToStr linha0Int
            let linha1Str = intToStr linha1Int
            let linha2Str = intToStr linha2Int
            let linha3Str = intToStr linha3Int
            y <- writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
            print rest
    else print "linha1"
    if linhaInt == 2
        then do
            let linha2Int = rest
            let linha0Str = intToStr linha0Int
            let linha1Str = intToStr linha1Int
            let linha2Str = intToStr linha2Int
            let linha3Str = intToStr linha3Int
            y <- writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
            print rest
    else print "linha2"
    if linhaInt == 3
        then do 
        let linha3Int = rest
        let linha0Str = intToStr linha0Int
        let linha1Str = intToStr linha1Int
        let linha2Str = intToStr linha2Int
        let linha3Str = intToStr linha3Int
        print linha3Str
        y <- writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
--     let linha3Int = rest
        print rest
    else print "linha3"
    novoRead <- readFile "palitos.txt"
    novoPalitosArr <- rList novoRead
    let max = maximum novoPalitosArr
    let maxStr = intToStr max
    print ("x: " ++ maxStr)
    if maximum novoPalitosArr /= 0
    then do 
        putStrLn "String"
        interface
    else
        putStrLn "Acavou!!"

main = do {
    putStrLn "Escolha um modo de jogo";
    escolha <- getLine;
    putStrLn ("Voce escolheu " ++ escolha);
    if escolha == "1"
        then
        interface;
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

-- data Palitos = Palitos { array :: IOArray Int Int }


-- menu = do
--     arr <- newArray (1,4) 1
--     let d = Palitos { array = arr }
--     writeArray (array d) 2 3
--     writeArray (array d) 3 5
--     writeArray (array d) 4 7
--     a <- readArray (array d) 3
--     b <- readArray (array d) 4
--     print (a,b)


menu = do
    x <- readFile "palitos.txt"
    palitos <- rList x
    print (palitos !! 0)
    y <- writeFile "palitos.txt" "[1,3,0,7]"
    print palitos
rList :: String -> IO [Int]	  
rList = readIO