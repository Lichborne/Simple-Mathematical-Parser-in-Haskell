# Simple-Mathematical-Parser-in-Haskell
This is a simple mathematical parser written in Haskell, written on the basis of a definition for a monadic Parser type provided by V. Bense of Eotvos Lorand University of Science. Please consult Regex.txt for RegEx specs. Use GHCI to interact with the program.

### Usage
Parsing (for expressions) can be validated in the following format: runParser pExpr "yourstringhere". E.g. runParser pExpr "11\*(12+4^2)"

### Syntax
Syntax is the standard Haskell syntax for multiplication, division, addition, subtraction, exponentiation, and the equivalence relation.

### Outputs
Outputs are constructed using the Maybe monad and the following type:

data Expr = Lit Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Eq Expr Expr

E.g. for the example above, the output will be Just (Mul (Lit 11) (Add (Lit 12) (Pow (Lit 4) (Lit 2))),"").

### Don't forget to be awesome.

