import Data.List
import Data.Char
import Control.Arrow (Arrow(first))
import Data.Attoparsec (string)

import Data.Array.MArray
import Data.Array.IO
import System.Random
import System.IO.Unsafe



strToInt x = read x :: Int
intToStr x = show x :: String
interface1 = do
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
    if linhaInt == 0 
        then do 
            let linha0Int = rest
            let linha0Str = intToStr linha0Int
            let linha1Str = intToStr linha1Int
            let linha2Str = intToStr linha2Int
            let linha3Str = intToStr linha3Int
            y <- writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
            print rest
    else print "" 
    if linhaInt == 1
        then do
            let linha1Int = rest
            let linha0Str = intToStr linha0Int
            let linha1Str = intToStr linha1Int
            let linha2Str = intToStr linha2Int
            let linha3Str = intToStr linha3Int
            y <- writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
            print rest
    else print ""
    if linhaInt == 2
        then do
            let linha2Int = rest
            let linha0Str = intToStr linha0Int
            let linha1Str = intToStr linha1Int
            let linha2Str = intToStr linha2Int
            let linha3Str = intToStr linha3Int
            y <- writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
            print rest
    else print ""
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
    else print ""
    playerRead <- readFile "palitos.txt"
    playerPalitosArr <- rList playerRead
    let max = maximum playerPalitosArr
    let maxStr = intToStr max
    print ("x: " ++ maxStr)
    if maximum playerPalitosArr /= 0
    then do 
        let r = unsafePerformIO (getStdRandom (randomR (0, 3))) :: Int
        print r
        interface1
    else do
        writeFile "palitos.txt" "[1,3,5,7]"
        putStrLn "Acabou!!"

main = do {
    putStrLn "Escolha um modo de jogo";
    escolha <- getLine;
    putStrLn ("Voce escolheu " ++ escolha);
    if escolha == "1"
        then
        interface1;
    else do
    putStrLn ("Acabou!");
}


rList :: String -> IO [Int]	  
rList = readIO