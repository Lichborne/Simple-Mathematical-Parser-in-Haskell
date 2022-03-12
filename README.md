# Simple-Mathematical-Parser-in-Haskell
This is a simple mathematical parser written in Haskell. Please consult Regex.txt for RegEx specs. Use GHCI to interact with the program.

Parsing (for expressions) can be validated in the following format: runParser pExpr "yourstringhere"
  
Syntax is the standard Haskell syntax for multiplication, division, addition, subtraction, exponentiation, and the equivalence relation.

Outputs are constructed using the following type:

data Expr = Lit Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Eq Expr Expr

Don't forget to be awesome.

