module Main where

import MyPol
  ( addPol,
    addPolynomialsInternal,
    derivative,
    multiplyPol,
    multiplyPolynomialInternal,
    normalize,
    normalizeInternal,
  )

main :: IO ()
main = do
  putStrLn "Deseja:"
  putStrLn "1.normalizar polinómios"
  putStrLn "2.adicionar polinómios"
  putStrLn "3.multiplicar polinómios"
  putStrLn "4.calcular a derivada de um polinómio"
  opçao <- getLine
  if opçao == "1"
    then do
      putStrLn "Insira o polinómio"
      polinómio <- getLine
      putStrLn (MyPol.normalize polinómio)
    else
      if opçao == "2"
        then do
          putStrLn "Insira o primeiro polinómio"
          polinómio1 <- getLine
          putStrLn "Insira o segundo polinómio"
          polinómio2 <- getLine
          putStrLn (MyPol.addPol polinómio1 polinómio2)
        else
          if opçao == "3"
            then do
              putStrLn "Insira o primeiro polinómio"
              polinómio1 <- getLine
              putStrLn "Insira o segundo polinómio"
              polinómio2 <- getLine
              putStrLn (MyPol.multiplyPol polinómio1 polinómio2)
            else
              if opçao == "4"
                then do
                  putStrLn "Insira o polinómio"
                  polinómio <- getLine
                  putStrLn "Insira variavel a derivar"
                  var <- getChar
                  putStrLn "Por fazer"
                  putStrLn (MyPol.derivative var polinómio)
                else putStrLn "Wrong number"
