import Data.List.Split

    
{- main
-}
main ::  IO()
main = do
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
                
                print (splitOn "+" pol)
                
    else if opçao == "2"
        then putStrLn opçao
    else if opçao == "3" 
        then putStrLn opçao
    else if opçao == "4" 
        then putStrLn opçao
    else putStrLn "Wrong number"

type Monomio = (Int, [(String, Int)])
type Polinomio = [Monomio]




