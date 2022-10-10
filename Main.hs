module Main where

import qualified MyLib
import qualified MyPol

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn "Deseja:"
    putStrLn "1.normalizar polinómios"
    putStrLn "2.adicionar polinómios"
    putStrLn "3.multiplicar polinómios"
    putStrLn "4.calcular a derivada de um polinómio"
    opçao <- getLine
    if opçao == "1"
      
    else if opçao == "2"
        then putStrLn opçao
    else if opçao == "3" 
        then putStrLn opçao
    else if opçao == "4" 
        then putStrLn opçao
    else putStrLn "Wrong number"
