{-# LANGUAGE DeriveFunctor #-}

module SimpleMathematicalParser where

import Control.Applicative
    ( optional, Alternative(empty, some, (<|>)) )
import Control.Monad
import Data.Char
import Data.Maybe
import Distribution.Types.ExposedModule (ExposedModule(exposedReexport))
import System.FilePath.Windows (pathSeparator)

-- Create Parser type
newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

-- Create necessary instances
instance Functor Parser where
    fmap f (Parser g) = Parser $ \x -> case g x of
        Nothing       -> Nothing
        Just (a, str) -> Just (f a, str)

instance Applicative Parser where
    pure a = Parser $ \x -> Just (a,x)
    (Parser f) <*> (Parser g) = Parser $ \x -> case f x of
        Nothing       -> Nothing
        Just (ab, str) -> case g str of
            Nothing         -> Nothing
            Just (a, str') -> Just (ab a, str')

instance Monad Parser where
    return = pure
    (Parser g) >>= f = Parser $ \x -> case g x of
        Nothing       -> Nothing
        Just (a, str) -> let (Parser h) = f a in h str

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser f) <|> p = Parser $ \x -> case f x of
        Nothing -> runParser p x
        result  -> result

-- Necessary functions
eof :: Parser ()
eof = Parser $ \x -> case x of
    [] -> Just ((),[])
    _  -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \x -> case x of
    (c:cs) | p c -> Just (c,cs)
    _            -> Nothing

char :: Char -> Parser ()
char c = () <$ satisfy (== c)

anyChar :: Parser Char
anyChar = satisfy $ const True

string :: String -> Parser ()
string = mapM_ char

-- "1,2,3,4,5,6apple" -> Just ([1,2,3,4,5,6], "apple")
-- "1apple"           -> Just ([1],"apple")
-- "apple"            -> Nothing
sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 pa psep = do
    a <- pa
    as <- many' (psep *> pa)
    return (a:as)

-- "apple" -> Just ([],"apple")
-- "1apple" -> Just ([1],"apple")
sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy pa psep = sepBy1 pa psep <|> pure []

some' :: Parser a -> Parser [a]
some' pa = sepBy1 pa (pure ())

many' :: Parser a -> Parser [a]
many' p@(Parser f) = Parser $ \x -> case f x of
    Nothing -> Just ([],x)
    Just (a,str) -> case runParser (many' p) str of
        Nothing -> Just ([a],str)
        Just (as,str') -> Just (a:as,str')


optional' :: Parser a -> Parser (Maybe a)
optional' (Parser f) = Parser $ \x -> case f x of
    Nothing -> Just (Nothing, x)
    Just (a,str) -> Just (Just a, str)

some_ :: Parser a -> Parser ()
some_ pa = () <$ some' pa

many_ :: Parser a -> Parser ()
many_ pa = () <$ many' pa

optional_ :: Parser a -> Parser ()
optional_ pa = () <$ optional' pa

{-
integer :: Parser Integer
integer = Parser $ \x -> case x of
    [] -> Nothing
    (c:cs) -> if isDigit c then Just (fromIntegral $ digitToInt c,cs) else if c == '-' then 
-}

-- "1234" -> [1,2,3,4] -> ((((1*10)+2)*10+3)*10+4
-- "321876123867asd" -> Just (321876123867, "asd")
integer :: Parser Integer
integer = do
    a <- optional (char '-')
    digits <- some (satisfy isDigit)
    let ints = map (fromIntegral.digitToInt) digits
    let num = foldl (\s x -> s*10+x) 0 ints
    case a of
        Just () -> return $ -num
        Nothing -> return num

-----------------------------------------
-- Element of a list ([...] in RegEx)
elemChar :: [Char] -> Parser Char
elemChar []     = empty
elemChar (x:xs) = satisfy (== x) <|> elemChar xs

-- runParser (elemChar "abc") "alma" == Just ('a',"lma")
-- runParser (elemChar "abc") "aaaa" == Just ('a',"aaa")
-- runParser (elemChar "abc") "barack" == Just ('b',"arack")
-- runParser (elemChar "abc") "citrom" == Just ('c',"itrom")
-- runParser (elemChar "acd") "barack" == Nothing

-- Whitespace, such as space, tab, newline (\s in RegEx)
whitespace :: Parser Char
whitespace = satisfy isSpace

-- A continous section of zero or more whitespace
ws :: Parser ()
ws = many_ whitespace

-- RegEx match checker
{-
match :: Parser a -> String -> Bool
match (Parser p) s = case p s of
    Just _ -> True
    _      -> False
-}
match :: Parser a -> String -> Bool
match = (isJust .) . runParser

-- match integer "3216534" == True
-- match integer "34879alm" == True

-- Implement the following parsers:

-- [a-z]
lowercase :: Parser Char
lowercase = undefined

-- Parse an email address of the following format and return the username!
-- [a-z]+@domain\.(com|org|hu)
emailUser :: Parser ()
emailUser = undefined

-- Parse a description of a person in the following format:
-- "John is an 11-year-old boy"
-- "Stepgen is a 24-year-old boy"
-- "Mary is a 29-year-old girl"
data Gender = Boy | Girl
data Person = MkPerson { name :: String, age :: Int, gender :: Gender }

person :: Parser Person
person = undefined

-- Same as p3, but count the number of elements in the list!
listLength :: Parser Int
listLength = undefined


----------------------------------------------------------
--                                                      --
--  Pasring and Evaluation of Mathematical Expressions  --
--                                                      --
----------------------------------------------------------

-- "12+23"
-- "19*8-7/2"

{-
    (+)
   /   \
 12    23
-}

data Expr = Lit Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Eq Expr Expr -- 12 = 12
          deriving Show

-- "12+23" == "       12    +    23    "
-- "(12+1)*(23-10)"

-- Tokenizing parsers

integer' :: Parser Integer
integer' = integer <* ws

char' :: Char -> Parser ()
char' f = char f <* ws

anyChar' :: Parser Char
anyChar' = anyChar <* ws

string' :: String -> Parser ()
string' s = string s <* ws

-- Lit :: Integer -> Expr
-- integer' :: Parser Integer
-- pAtom :: Parser Expr

pAtom :: Parser Expr -- numbers, bracketed expressions
pAtom = Lit <$> integer' -- fmap Lit integer'
    <|> (char' '(' *> pEq <* char' ')')

-- "3 ^ 4 ^ 2" == "3 ^ (4 ^ 2)"
-- "3 ^ (1+2) ^ 3"
pPow :: Parser Expr
pPow = infixRight Pow pAtom (char' '^')

pMul :: Parser Expr
pMul = chainLeft1 pPow (Mul <$ char' '*' <|> Div <$ char' '/')

pAdd :: Parser Expr
pAdd = chainLeft1 pMul (Add <$ char' '+' <|> Sub <$ char' '-')

pEq :: Parser Expr -- non-directional, using right
pEq = infixNonAssoc Eq pAdd (char' '=')
-- "12 + 23 = 4 * 5"
 
pExpr :: Parser Expr
pExpr = ws *> pEq <* eof

-- "3 + 4 + 2 + 1 + 0" ->Add (Add (Add (Lit 3) (Lit 4)) (Lit 2)) (Lit 1))
infixLeft :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
infixLeft f pa psep = do
    a <- sepBy1 pa psep
    case a of 
        x:y:ys -> pure (foldl f x (y:ys))
        [x]      -> pure x 
        []       -> empty

chainLeft1 ::  Parser a -> Parser (a -> a -> a) -> Parser a
chainLeft1 pa puff = do
    a <- pa
    go a where 
    go a = (do
        op <- puff
        b <- pa
        go (op a b)) <|> pure a

-- "3 ^ 4 ^ 2" -> Pow (Lit 3) (Pow (Lit 4) (Lit 2))
infixRight :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
infixRight f pa psep = do
    a <- pa
    (do
        psep
        b <- infixRight f pa psep
        pure (f a b)) <|> pure a

prefixAssoc :: (a -> a) -> Parser op -> Parser a -> Parser a
prefixAssoc f pop pa = undefined

infixNonAssoc :: (a -> a -> a) -> Parser a -> Parser sep -> Parser a
infixNonAssoc f pa psep = do
    a <- sepBy1 pa psep
    case a of 
        [x,y]      -> pure (f x y)
        [x]        -> pure x
        _          -> empty

prefixNonAssoc :: (a -> a) -> Parser op -> Parser a -> Parser a
prefixNonAssoc f pop pa = undefined

{- 
to do when extending parser

postfixAssoc
postfixAssoc

postfixNonAssoc
postfixNonAssoc -}
