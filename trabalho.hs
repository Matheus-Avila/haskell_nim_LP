import Data.List
import Data.Char
import Control.Arrow (Arrow(first))
import Data.Attoparsec (string)

import Data.Array.MArray
import Data.Array.IO
import System.Random
import System.IO.Unsafe
import Control.Monad
import Text.Read (Lexeme(String))
import Language.Haskell.TH (Lit(StringL, IntegerL))


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
            writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
            print rest
    else print "" 
    if linhaInt == 1
        then do
            let linha1Int = rest
            let linha1Str = intToStr linha1Int
            writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
            print rest
    else print ""
    if linhaInt == 2
        then do
            let linha2Int = rest
            let linha2Str = intToStr linha2Int
            writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
            print rest
    else print ""
    if linhaInt == 3
        then do 
        let linha3Int = rest
        let linha3Str = intToStr linha3Int
        print linha3Str
        writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
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
        let loop = do
            let random = unsafePerformIO (getStdRandom (randomR (0, 3))) :: Int
            let palitosIndice = playerPalitosArr !! random
            let randomStr = intToStr random :: String
            -- _ <- return randomStr
            -- return random :: Int
            if palitosIndice == 0
                then loop 
                else writeFile "pc.txt" randomStr
        loop
        readPc <- readFile "pc.txt"
        let linhaInt = strToInt readPc
        let valMax = playerPalitosArr !! linhaInt
        let palitoRandom = unsafePerformIO (getStdRandom (randomR (1, valMax))) :: Int
        let palitosRestantes = valMax - palitoRandom
        let palitosRestantesStr = intToStr palitosRestantes
        let pclinha0Int = playerPalitosArr !! 0
        let pclinha1Int = playerPalitosArr !! 1
        let pclinha2Int = playerPalitosArr !! 2
        let pclinha3Int = playerPalitosArr !! 3
        let pclinha0Str = intToStr pclinha0Int
        let pclinha1Str = intToStr pclinha1Int
        let pclinha2Str = intToStr pclinha2Int
        let pclinha3Str = intToStr pclinha3Int
        if linhaInt == 0 
        then do 
            let pclinha0Int = palitosRestantes
            let pclinha0Str = intToStr pclinha0Int
            writeFile "palitos.txt" ("[" ++ pclinha0Str ++ ","++ pclinha1Str ++ ","++ pclinha2Str ++ ","++ pclinha3Str ++ "]")
            print rest
        else print ""
        if linhaInt == 1 
        then do 
            let pclinha1Int = palitosRestantes
            let pclinha1Str = intToStr pclinha1Int
            writeFile "palitos.txt" ("[" ++ pclinha0Str ++ ","++ pclinha1Str ++ ","++ pclinha2Str ++ ","++ pclinha3Str ++ "]")
            print rest
        else print ""
        if linhaInt == 2 
        then do 
            let pclinha2Int = palitosRestantes
            let pclinha2Str = intToStr pclinha2Int
            writeFile "palitos.txt" ("[" ++ pclinha0Str ++ ","++ pclinha1Str ++ ","++ pclinha2Str ++ ","++ pclinha3Str ++ "]")
            print rest
        else print ""
        if linhaInt == 3
        then do 
            let pclinha3Int = palitosRestantes
            let pclinha3Str = intToStr pclinha3Int
            writeFile "palitos.txt" ("[" ++ pclinha0Str ++ ","++ pclinha1Str ++ ","++ pclinha2Str ++ ","++ pclinha3Str ++ "]")
            print rest
        else print ""
        pcRead <- readFile "palitos.txt"
        pcPalitosArr <- rList pcRead
        let max = maximum pcPalitosArr
        let maxStr = intToStr max
        print ("x: " ++ maxStr)
        if maximum pcPalitosArr /= 0
            then interface1
        else do
            writeFile "palitos.txt" "[1,3,5,7]"
            putStrLn "Voce perdeu!!"
    else do
        writeFile "palitos.txt" "[1,3,5,7]"
        putStrLn "Voce ganhou!!"

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