import Data.List.Split as S
import qualified Data.Text as T


type Polinomio = (Int, String, Int)

listToTuple :: [a] -> (a,a,a)
listToTuple [x,y,z] = (x,y,z)

listToTuple2 :: [a] -> (a,a)
listToTuple2 [x,y] = (x,y)

polinomios ::  IO()
polinomios = do
    putStrLn "Deseja:"
    putStrLn "1.normalizar polinómios"
    putStrLn "2.adicionar polinómios"
    putStrLn "3.multiplicar polinómios"
    putStrLn "4.calcular a derivada de um polinómio"
    opçao <- getLine
    if opçao == "1" 
        then do putStrLn "Digite o primeiro polinómio"
                pol <- getLine
                --Polinomio = listToTuple splitOn "+" pol
                --putStrLn (trim " dsa ")
               
                
                print (S.splitOn "+" pol)
                
    else if opçao == "2" 
        then putStrLn opçao
    else if opçao == "3" 
        then putStrLn opçao
    else if opçao == "4" 
        then putStrLn opçao
    else putStrLn "Wrong number"

{-Normalizar monomio em tuplo 
Ex. 2x -> (2,x,1)
    2xy^2
-}
normalizeMonomial :: String -> (x,y,z)
normalizeMonomial monomio = 

