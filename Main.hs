module Main where

import qualified MyLib
import qualified MyPol

main :: IO ()
main = do
    putStrLn "Deseja:"
    putStrLn "1.normalizar polinómios"
    putStrLn "2.adicionar polinómios"
    putStrLn "3.multiplicar polinómios"
    putStrLn "4.calcular a derivada de um polinómio"
    opçao <- getLine
    if opçao == "1" then do
      putStrLn "Insira o polinómio"
      polinómio <- getLine
      putStrLn (MyPol.polynomialToString (MyPol.normalizePolynomial (MyPol.stringToPolynomial polinómio)))
    else if opçao == "2" then do
      putStrLn "Insira o primeiro polinómio"
      polinómio1 <- getLine
      putStrLn "Insira o segundo polinómio"
      polinómio2 <- getLine
      putStrLn "Por fazer"
      --putStrLn (MyPol.polynomialToString (MyPol.addPolynomials (MyPol.stringToPolynomial polinómio1) (MyPol.stringToPolynomial polinómio2)))
    else if opçao == "3" then do
      putStrLn "Insira o primeiro polinómio"
      polinómio1 <- getLine
      putStrLn "Insira o segundo polinómio"
      polinómio2 <- getLine
      putStrLn (MyPol.polynomialToString (MyPol.multiplyPolynomial (MyPol.stringToPolynomial polinómio1) (MyPol.stringToPolynomial polinómio2)))
    else if opçao == "4" then do
      putStrLn "Insira o polinómio"
      polinómio <- getLine
      putStrLn "Por fazer"
      --putStrLn (MyPol.polynomialToString (MyPol.derivativePolynomial (MyPol.stringToPolynomial polinómio)))
    else putStrLn "Wrong number"
