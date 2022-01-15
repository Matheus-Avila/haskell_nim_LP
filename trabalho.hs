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

import Numeric
import GHC.Base
import Test.QuickCheck (vector)

strToInt x = read x :: Int
intToStr x = show x :: String


toBin 0 = [0]
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]

toInt xs = 4*(xs !! 0 `mod` 2) + 2* (xs !! 1 `mod` 2) + xs !! 2 `mod` 2

interface2 = do
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
    else print () 
    if linhaInt == 1
        then do
            let linha1Int = rest
            let linha1Str = intToStr linha1Int
            writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
            print rest
    else print ()
    if linhaInt == 2
        then do
            let linha2Int = rest
            let linha2Str = intToStr linha2Int
            writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
            print rest
    else print ()
    if linhaInt == 3
        then do 
        let linha3Int = rest
        let linha3Str = intToStr linha3Int
        print linha3Str
        writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
        print rest
    else print ()
    playerRead <- readFile "palitos.txt"
    playerPalitosArr <- rList playerRead
    putStrLn ("Palitos apos jogador: "++ show playerPalitosArr)
    let max = maximum playerPalitosArr
    let maxStr = intToStr max
    if maximum playerPalitosArr /= 0
    then do 
        readPlayer <- readFile "palitos.txt"
        playerPalitosArr <- rList readPlayer
        -- Pega cada linha
        let pclinha0Int = playerPalitosArr !! 0
        let pclinha1Int = playerPalitosArr !! 1
        let pclinha2Int = playerPalitosArr !! 2
        let pclinha3Int = playerPalitosArr !! 3
        let linha0Str = intToStr pclinha0Int
        let linha1Str = intToStr pclinha1Int
        let linha2Str = intToStr pclinha2Int
        let linha3Str = intToStr pclinha3Int
        -- Coloca em um vetor
        let binPalitos0aux = toBin pclinha0Int
        let binPalitos1aux = toBin pclinha1Int
        let binPalitos2aux = toBin pclinha2Int
        let binPalitos3aux = toBin pclinha3Int
        -- Joga fora o primeiro elemento de cada array
        let binPalitos0tail = tail binPalitos0aux
        let binPalitos1tail = tail binPalitos1aux
        let binPalitos2tail = tail binPalitos2aux
        let binPalitos3tail = tail binPalitos3aux
        -- Garantindo que todos os vetores tem no minimo 3 posicoes
        let binPalitos3 = reverse([0,0,0]++binPalitos3tail)
        let binPalitos2 = reverse([0,0,0]++binPalitos2tail)
        let binPalitos1 = reverse([0,0,0]++binPalitos1tail)
        let binPalitos0 = reverse([0,0,0]++binPalitos0tail)
        -- Garante que vamos pegar apenas os 3 primeiros digitos binarios(que sao os que importam ja que o maior valor é 7= 1 1 1)
        let varLimitadora = [0,0,0] 
        -- Verifica quanto deve ser subtraido para a soma dar par
        let sumAux0 = zipWith (+) binPalitos0 varLimitadora
        let sumAux1 = zipWith (+) sumAux0 binPalitos3
        let sumAux2 = zipWith (+) sumAux1 binPalitos2
        let sumAux3 = zipWith (+) sumAux2 binPalitos1
        let numReverse = reverse(sumAux3)
        let numChave = toInt numReverse
        let numChaveStr = intToStr numChaveStr
        putStrLn (show numReverse)
        putStrLn ("Chave::  "++show numChave)
        
        if numChave <= pclinha3Int
        then do
            let rest = pclinha3Int - numChave
            let restStr = intToStr rest
            writeFile "pc.txt" ("[3," ++ restStr++"]")
            putStrLn restStr
        else print "*"

        if numChave <= pclinha2Int
        then do
            let rest = pclinha2Int - numChave
            let restStr = intToStr rest
            writeFile "pc.txt" ("[2," ++ restStr++"]")
            putStrLn restStr
        else print "*"
        
        if numChave <= pclinha1Int
        then do
            let rest = pclinha1Int - numChave
            let restStr = intToStr rest
            writeFile "pc.txt" ("[1," ++ restStr++"]")
            putStrLn restStr
        else print "*"

        if numChave <= pclinha0Int
        then do
            let rest = pclinha0Int - numChave
            let restStr = intToStr rest
            writeFile "pc.txt" ("[0," ++ restStr++"]")
            putStrLn restStr
        else print "*"
        if pclinha0Int == 0 && pclinha1Int == 0 && pclinha2Int == 1 && pclinha3Int > pclinha2Int
        then do 
            writeFile "pc.txt" ("[3,1]")
            print "*"
        else print "*"

        if pclinha0Int == 1 && pclinha1Int == 0 && pclinha2Int == 0 && pclinha3Int > pclinha0Int
        then do 
            writeFile "pc.txt" ("[3,1]")
            print "*"
        else print "*"
        
        if pclinha0Int == 0 && pclinha1Int == 1 && pclinha2Int == 0 && pclinha3Int > pclinha1Int
        then do 
            writeFile "pc.txt" ("[3,1]")
            print "*"
        else print "*"
        if pclinha0Int == 0 && pclinha1Int == 0 && pclinha3Int == 1 && pclinha2Int > pclinha3Int
        then do 
            writeFile "pc.txt" ("[2,1]")
            print "*"
        else print "*"

        if pclinha0Int == 1 && pclinha1Int == 0 && pclinha3Int == 0 && pclinha2Int > pclinha0Int
        then do 
            writeFile "pc.txt" ("[2,1]")
            print "*"
        else print "*"
        
        if pclinha0Int == 0 && pclinha1Int == 1 && pclinha3Int == 0 && pclinha2Int > pclinha1Int
        then do 
            writeFile "pc.txt" ("[2,1]")
            print "*"
        else print "*"
        if pclinha0Int == 0 && pclinha2Int == 0 && pclinha3Int == 1 && pclinha1Int > pclinha3Int
        then do 
            writeFile "pc.txt" ("[1,1]")
            print "*"
        else print "*"

        if pclinha0Int == 1 && pclinha2Int == 0 && pclinha3Int == 0 && pclinha1Int > pclinha0Int
        then do 
            writeFile "pc.txt" ("[1,1]")
            print "*"
        else print "*"
        
        if pclinha0Int == 0 && pclinha2Int == 1 && pclinha3Int == 0 && pclinha1Int > pclinha2Int
        then do 
            writeFile "pc.txt" ("[1,1]")
            print "*"
        else print "*"
        if pclinha1Int == 0 && pclinha2Int == 0 && pclinha3Int == 1 && pclinha0Int > pclinha3Int
        then do 
            writeFile "pc.txt" ("[0,1]")
            print "*"
        else print "*"

        if pclinha1Int == 1 && pclinha2Int == 0 && pclinha3Int == 0 && pclinha0Int > pclinha1Int
        then do 
            writeFile "pc.txt" ("[0,1]")
            print "*"
        else print "*"
        
        if pclinha1Int == 0 && pclinha2Int == 1 && pclinha3Int == 0 && pclinha0Int > pclinha2Int
        then do 
            writeFile "pc.txt" ("[0,1]")
            print "*"
        else print "*"
        
        readPc <- readFile "pc.txt"
        vetPc <- rList readPc
        let pos = vetPc !! 0
        let rest = vetPc !! 1
        let restStr = intToStr rest
        if pos == 0 
            then do
                let linha0Int = rest
                let linha0Str = intToStr linha0Int
                writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
                print rest
        else print () 
        if pos == 1
            then do
                let linha1Int = rest
                let linha1Str = intToStr linha1Int
                writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
                print rest
        else print ()
        if pos == 2
            then do
                let linha2Int = rest
                let linha2Str = intToStr linha2Int
                writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
                print rest
        else print ()
        if pos == 3
            then do 
            let linha3Int = rest
            let linha3Str = intToStr linha3Int
            print linha3Str
            putStrLn ("linha 2:: " ++ linha2Str)
            writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
    --     let linha3Int = rest
            print rest
        else print ()
        print "*****************"
        pcRead <- readFile "palitos.txt"
        pcPalitosArr <- rList pcRead
        let max = maximum pcPalitosArr
        let maxStr = intToStr max
        print ("maxStr: " ++ maxStr)
        if maximum pcPalitosArr /= 0
            then interface2
        else do
            writeFile "palitos.txt" "[1,3,5,7]"
            putStrLn "Voce perdeu!!"
        
    else do
        writeFile "palitos.txt" "[1,3,5,7]"
        putStrLn "Voce ganhou!!"
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
    else print () 
    if linhaInt == 1
        then do
            let linha1Int = rest
            let linha1Str = intToStr linha1Int
            writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
            print rest
    else print ()
    if linhaInt == 2
        then do
            let linha2Int = rest
            let linha2Str = intToStr linha2Int
            writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
            print rest
    else print ()
    if linhaInt == 3
        then do 
        let linha3Int = rest
        let linha3Str = intToStr linha3Int
        print linha3Str
        writeFile "palitos.txt" ("[" ++ linha0Str ++ ","++ linha1Str ++ ","++ linha2Str ++ ","++ linha3Str ++ "]")
--     let linha3Int = rest
        print rest
    else print ()
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
        else print ()
        if linhaInt == 1 
        then do 
            let pclinha1Int = palitosRestantes
            let pclinha1Str = intToStr pclinha1Int
            writeFile "palitos.txt" ("[" ++ pclinha0Str ++ ","++ pclinha1Str ++ ","++ pclinha2Str ++ ","++ pclinha3Str ++ "]")
            print rest
        else print ()
        if linhaInt == 2 
        then do 
            let pclinha2Int = palitosRestantes
            let pclinha2Str = intToStr pclinha2Int
            writeFile "palitos.txt" ("[" ++ pclinha0Str ++ ","++ pclinha1Str ++ ","++ pclinha2Str ++ ","++ pclinha3Str ++ "]")
            print rest
        else print ()
        if linhaInt == 3
        then do 
            let pclinha3Int = palitosRestantes
            let pclinha3Str = intToStr pclinha3Int
            writeFile "palitos.txt" ("[" ++ pclinha0Str ++ ","++ pclinha1Str ++ ","++ pclinha2Str ++ ","++ pclinha3Str ++ "]")
            print rest
        else print ()
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
    putStrLn "1- modo fácil";
    putStrLn "2- modo difícil";
    escolha <- getLine;
    putStrLn ("Voce escolheu " ++ escolha);
    if escolha == "1"
        then do 
            writeFile "palitos.txt" "[1,3,5,7]"
            interface1;
    else if escolha == "2" 
        then do
        writeFile "palitos.txt" "[1,3,5,7]"
        interface2;
        else
            putStrLn "Valor inválido";
}   


rList :: String -> IO [Int]
rList = readIO