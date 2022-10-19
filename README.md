# PFL T2G06
## Haskell Programming (First project work)

## Getting started
1. To run our project run the following command in the root directory of the project:

```ghci```

2. Then load the main file:

```:l src/Main.hs```

3. Then run the main function:

```:main```

 3. or choose from various other functions:

```:derivative 'x' '3x'```

```:multiplyPol '3x' '2y'```

```:normalize '3x +2x + 4y'```

```:addPol '3x' '2y'```

## Internal Polynomial structure

We decided that our polynomial structure would be a list of monomials.
Therefore our monomial is a structure where the first element is the coeficient and the second element is a list of tuples, where the first element of the tuple is the variable and the second element is the power. For example, the polynomial 3x^2 - y + 1 would be represented as [(3, [('x', 2)]), (-1, [('y', 1)]), (1, [])]. We chose this representation because it is easy to work with and it is easy to convert to a string.

## Short description of each function

### Normalize

### Add

### Multiply
The multiply function takes two polynomials and multiplies them together. It does this by multiplying each monomial of the first polynomial with the second polynomial. It then adds the results together.

### Derivative
The derivative function, iterates through the polynomial and derivates each monomial. To differenciate a monomial, we multiply the coeficient with the power and then subtract 1 from the power. If the coeficient is 0, we remove the monomial from the polynomial, through the normalize function. 

### Authors

---

GROUP0206, 23/10/2022

* João de Oliveira Gigante Pinheiro, up202008133@up.pt
* Ricardo Almeida Cavalheiro, up202005103@up.pt



