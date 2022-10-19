module Main where

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
      putStrLn (MyPol.addPol polinómio1 polinómio2)
    else if opçao == "3" then do
      putStrLn "Insira o primeiro polinómio"
      polinómio1 <- getLine
      putStrLn "Insira o segundo polinómio"
      polinómio2 <- getLine
      putStrLn (MyPol.multiplyPol polinómio1 polinómio2)
    else if opçao == "4" then do
      putStrLn "Insira o polinómio"
      polinómio <- getLine
      putStrLn "Por fazer"
      --putStrLn (MyPol.polynomialToString (MyPol.derivativePolynomial (MyPol.stringToPolynomial polinómio)))
    else putStrLn "Wrong number"

{-Testing-}
testing :: IO ()
testing =  do
                  putStr  "\n\n--------Testing normalizing polynomials--------\n\n"
                  putStr  ("x^2 + 5x + 3x + 3x^2  normalized is  " ++ MyPol.normalize "x^2 + 5x + 3x + 3x^2")
                  putStr  "\n"
                  if "4x^2 + 8x" == MyPol.normalize "x^2 + 5x + 3x + 3x^2" then putStr "Test passed\n" else putStr "Test failed\n"

                  putStr "[(1,[('x', 2)]), (5,[('x',1)]),(3,[('x',1)]),(3,[('x',2)])] normalized is "
                  print (MyPol.normalizeInternal [(1,[('x', 2)]), (5,[('x',1)]),(3,[('x',1)]),(3,[('x',2)])])
                  if [(4,[('x',2)]),(8,[('x',1)])] == MyPol.normalizeInternal [(1,[('x', 2)]), (5,[('x',1)]),(3,[('x',1)]),(3,[('x',2)])] then putStr "Test passed\n" else putStr "Test failed\n"

                  putStr  ("2x^2y + 5yx^2 + 5xy + 2 yx + 3  normalized is  " ++ MyPol.normalize "2x^2y + 5yx^2 + 5xy + 2 yx + 3")
                  putStr  "\n"
                  if "7x^2y + 7xy + 3" == MyPol.normalize "2x^2y + 5yx^2 + 5xy + 2 yx + 3" then putStr "Test passed\n" else putStr "Test failed\n"

                  putStr "[(2,[('x', 2),('y',1)]), (5,[('y',1),('x',2)]),(5,[('x',1),('y',1)]),(2,[('y',1),('x',1)]),(3,[])] normalized is "
                  print (MyPol.normalizeInternal [(2,[('x', 2),('y',1)]), (5,[('y',1),('x',2)]),(5,[('x',1),('y',1)]),(2,[('y',1),('x',1)]),(3,[])])
                  if [(7,[('x',2),('y',1)]),(7,[('x',1),('y',1)]),(3,[])] == MyPol.normalizeInternal [(2,[('x', 2),('y',1)]), (5,[('y',1),('x',2)]),(5,[('x',1),('y',1)]),(2,[('y',1),('x',1)]),(3,[])] then putStr "Test passed\n" else putStr "Test failed\n"


      
                  putStr  "\n\n--------Testing adding polynomials--------\n\n"
                  putStr  ("x^2 + 2x + 1  +  5x^2 + 3x + 2  =  " ++ MyPol.addPol "x^2 + 2x + 1" "5x^2 + 3x + 2 ")
                  putStr  "\n"
                  if "6x^2 + 5x + 3" == MyPol.addPol "x^2 + 2x + 1" "5x^2 + 3x + 2 " then putStr "Test passed\n" else putStr "Test failed\n"

                  putStr "[(2,[('x', 1)]), (3,[('x',0)])] + [(5,[('x', 2)]), (3,[('x',1)])] = "
                  print (MyPol.addPolynomialsInternal [(2,[('x', 1)]), (3,[('x',0)])] [(5,[('x', 2)]), (3,[('x',1)])] )
                  if [(5,[('x',2)]),(5,[('x',1)]),(3,[])] == MyPol.addPolynomialsInternal [(2,[('x', 1)]), (3,[('x',0)])] [(5,[('x', 2)]), (3,[('x',1)])] then putStr "Test passed\n" else putStr "Test failed\n"

                  putStr  ("x^2y + 2yx + 3yx^2 + 1 + 6x^2y + 3xy + 2x = " ++ MyPol.addPol "x^2y + 2yx + 3yx^2 + 1" "6x^2y + 3xy + 2x")
                  putStr "\n"
                  if "10x^2y + 5xy + 2x + 1" == MyPol.addPol "x^2y + 2yx + 3yx^2 + 1" "6x^2y + 3xy + 2x" then putStr "Test passed\n" else putStr "Test failed\n"

                  putStr "[(1,[('x', 2),('y',1)]), (2,[('y',1),('x',1)]),(3,[('y',1),('x',2)]),(1,[])] + [(6,[('x', 2),('y',1)]), (3,[('x',1),('y',1)]),(2,[('x',1)])] = "
                  print (MyPol.addPolynomialsInternal [(1,[('x', 2),('y',1)]), (2,[('y',1),('x',1)]),(3,[('y',1),('x',2)]),(1,[])] [(6,[('x', 2),('y',1)]), (3,[('x',1),('y',1)]),(2,[('x',1)])] )
                  if [(10,[('x',2),('y',1)]),(5,[('x',1),('y',1)]),(2,[('x',1)]),(1,[])] == MyPol.addPolynomialsInternal  [(1,[('x', 2),('y',1)]), (2,[('y',1),('x',1)]),(3,[('y',1),('x',2)]),(1,[])] [(6,[('x', 2),('y',1)]), (3,[('x',1),('y',1)]),(2,[('x',1)])] then putStr "Test passed\n" else putStr "Test failed\n"

                  
                  
                  putStr  "\n\n--------Testing multiplying polynomials--------\n\n"
                  putStr  ("x^2 + 2x + 1  *  2x + 2  =  " ++ MyPol.multiplyPol "x^2 + 2x + 1" "2x + 2")
                  putStr  "\n"
                  if "2x^3 + 6x^2 + 6x + 2" == MyPol.multiplyPol "x^2 + 2x + 1" "2x + 2" then putStr "Test passed\n" else putStr "Test failed\n"

                  putStr "[(1,[('x', 2)]), (2,[('x',1)]),(1,[])] * [(2,[('x', 1)]), (2,[])] = "
                  print (MyPol.multiplyPolynomialInternal [(1,[('x', 2)]), (2,[('x',1)]),(1,[])] [(2,[('x', 1)]), (2,[])] )
                  if [(2,[('x',3)]),(6,[('x',2)]), (6,[('x',1)]),(2,[])] == MyPol.multiplyPolynomialInternal [(1,[('x', 2)]), (2,[('x',1)]),(1,[])] [(2,[('x', 1)]), (2,[])] then putStr "Test passed\n" else putStr "Test failed\n"


                  putStr  ("2x^2y + 3yx^2 + 3yx + 2xy * 2x  =  " ++ MyPol.multiplyPol "2x^2y + 3yx^2 + 3yx + 2xy" "2x")
                  putStr  "\n"
                  if "10x^3y + 10x^2y" == MyPol.multiplyPol "2x^2y + 3yx^2 + 3yx + 2xy" "2x" then putStr "Test passed\n" else putStr "Test failed\n"

                  putStr "[(2,[('x', 2),('y',1)]), (3,[('y', 1),('x',2)]),(3,[('y', 1),('x',1)]), (2,[('x', 1),('y',1)])] * [(2,[('x', 1)])] = "
                  print (MyPol.multiplyPolynomialInternal [(2,[('x', 2),('y',1)]), (3,[('y', 1),('x',2)]),(3,[('y', 1),('x',1)]), (2,[('x', 1),('y',1)])] [(2,[('x', 1)])] )
                  if [(10,[('x',3),('y',1)]),(10,[('x',2),('y',1)])] == MyPol.multiplyPolynomialInternal [(2,[('x', 2),('y',1)]), (3,[('y', 1),('x',2)]),(3,[('y', 1),('x',1)]), (2,[('x', 1),('y',1)])] [(2,[('x', 1)])] then putStr "Test passed\n" else putStr "Test failed\n"