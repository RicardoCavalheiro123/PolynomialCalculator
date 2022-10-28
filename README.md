# Polynomial Calculator
## Haskell Programming (First project work)

## Getting started
1. To run our project run the following command in the root directory of the project:

```ghci```

2. Then load the main file:

```:l Main.hs```

 Then run the main function:

```:main```

 3. or run the following functions:

```derivative 'x' "3x"```

```multiplyPol "3x" "2y"```

```normalize "3x +2x + 4y"```

```addPol "3x" "2y"```

## Internal Polynomial structure

We decided that our polynomial structure would be a list of monomials.
Therefore our monomial is a structure where the first element is the coeficient and the second element is a list of tuples, where the first element of the tuple is the variable and the second element is the power. For example, the polynomial 3x^2 - y + 1 would be represented as [(3, [('x', 2)]), (-1, [('y', 1)]), (1, [])]. We chose this representation because it is easy to work with and it is easy to convert to a string.

## Short description of each function

#### __Normalize__
The normalize function takes a polynomial and normalizes it. It adds the coeficients of the monomials with the same variables as well as cleans the polynomial. Also the resulting polynomial is sorted.

#### __Add__
The add function takes two polynomial and adds them. It is similar to normalizing a polynomial but with 2 polynomials. It is also cleaned and sorted.

#### __Multiply__
The multiply function takes two polynomials and multiplies them together. It does this by multiplying each monomial of the first polynomial with the second polynomial. It then adds the results together.

#### __Derivative__
The derivative function, iterates through the polynomial and derivates each monomial. To differenciate a monomial, we multiply the coeficient with the power and then subtract 1 from the power. If the coeficient is 0, we remove the monomial from the polynomial, through the normalize function. 

## Testing
To run the tests, run the following command in the root directory of the project:

```testing```

after loading the MyTest.hs file:

```:l MyTest.hs```

or run for the quickcheck tests by running:

```quickCheck prop```

## Dependencies

Needed packages to run our program:

Quick Check:
```cabal install QuickCheck```
```cabal install --lib QuickCheck```
```:set -package QuickCheck```




### Authors

---

GROUP0206, 23/10/2022

* João de Oliveira Gigante Pinheiro, up202008133@up.pt
* Ricardo Almeida Cavalheiro, up202005103@up.pt



