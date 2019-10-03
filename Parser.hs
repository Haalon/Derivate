{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser where
import Data.Char
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Monad.Reader
import Text.Read (readMaybe)

newtype Parser r a = Parser { unParser :: r -> String -> [(a,String)] }

-- does exactly what it says
-- but you can also run parser at lower level by (unParser :: Parser a -> String -> [(a,String)])
-- unParser will return incorrect parses instead of raising exepctions
runParser :: Parser r a -> r -> String -> a
runParser m env s = case unParser m env s of    
    [(res, [])]   -> res
    [(res, rest)] -> error $ "Parser did not consume entire stream. Rest is: " ++ rest
    _             -> error "Parser error."


-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Functor (Parser r) where
  fmap f (Parser rcs) = Parser (\r s -> [(f a, b) | (a, b) <- rcs r s])

instance Applicative (Parser r) where
  pure a = Parser (\r s-> [(a,s)])
  -- (pf <*> pa) creates a parser that at first parses pf to get  functions
  -- then parses pa on the remaining input and applies those functions to these results
  (Parser rcs1) <*> (Parser rcs2) = Parser (\r s -> [(f a, s2) | (f, s1) <- rcs1 r s, (a, s2) <- rcs2 r s1])


-- (p >>= fp) applies fp to every result of p parser, and then combines the result
bind :: Parser r a -> (a -> Parser r b) -> Parser r b
bind p f = Parser $ \r s -> concatMap (\(a, s') -> unParser (f a) r s') $ unParser p r s

instance Monad (Parser r) where
    return = pure
    (>>=)  = bind

instance MonadReader r (Parser r) where
    ask = Parser $ \r s -> [(r,s)]
    local f p = Parser $ \r s -> unParser p (f r) s

-- does exactly what it says
failure :: Parser r a
failure = Parser (\r s -> [])

-- (option l r) runs l, and only if it fails, runs r. (<|>) in infix 
option :: Parser r a -> Parser r a -> Parser r a
option  p q = Parser $ \r s ->
    case unParser p r s of
        []     -> unParser q r s
        res    -> res

instance Alternative (Parser r) where
    empty = failure
    (<|>) = option

-------------------------------------------------------------------------------
-- Basis
-------------------------------------------------------------------------------

-- parse first char
item :: Parser r Char
item = Parser $ \ _ s ->
    case s of
        []     -> []
        (c:cs) -> [(c,cs)]

-- Look-ahead: returns the part of the input that is left, without consuming it.
look :: Parser r String
look = Parser $ \r s -> [(s,s)]

-- run two parsers on the same input independently, then combine the results
(+++) :: Parser r a -> Parser r a -> Parser r a
(+++) p q = Parser (\r s -> unParser p r s ++ unParser q r s)

-- try given parser, without consuming input
try :: Parser r a -> Parser r a
try (Parser rcs) = Parser $ \r s -> [(a, s) | (a, _) <- rcs r s]

-------------------------------------------------------------------------------
-- Abstract
-------------------------------------------------------------------------------
fromMaybe :: Alternative m => Maybe a -> m a
fromMaybe Nothing  = empty
fromMaybe (Just a) = pure a

(<++>) :: (Applicative f, Monoid b) => f b -> f b -> f b
(<++>) ap1 ap2 = mappend <$> ap1 <*> ap2
-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

-- parses char if it satisfies the predicate
satisfy :: (Char -> Bool) -> Parser r Char
satisfy p = item >>= \c ->
    if p c
        then return c
        else failure

satisfyGen :: (a -> Bool) -> Parser r a -> Parser r a
satisfyGen f p = p >>= \a ->
    if f a
        then return a
        else failure

-- same as item but with 1-length string
itemS :: Parser r String
itemS = (:[]) <$> item

-- parses one of the characters from list
oneOf :: [Char] -> Parser r Char
oneOf s = satisfy (flip elem s)

-- parses one of the strings from list
oneOfStr :: [String] -> Parser r String
oneOfStr [] = failure
oneOfStr (s:ss) = string s +++ oneOfStr ss

-- Parses zero or more occurrences of the given parser.
-- Returns all intermediate parses
-- do not confuse with (many :: Parser r a -> Parser r [a]) from Control.Applicative
-- (many p) returns only the final result
manyL :: Parser r a -> Parser r [a]
manyL p = return [] +++ someL p


-- Parses one or more occurrences of the given parser.
-- Returns all intermediate parses
-- do not confuse with (some :: Parser r a -> Parser r [a]) from Control.Applicative
-- (some p) returns only the final result
someL :: Parser r a -> Parser r [a]
someL p = (:) <$> p <*> (manyL p)


-- (chainl p op) parses zero or more occurrences of p, separated by op. 
-- Returns a value produced by a LEFT associative application of all functions returned by op.
chainl1 :: Parser r a -> Parser r (a -> a -> a) -> Parser r a
p `chainl1` op = p >>= rest
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a
-- (chainl p op) parses zero or more occurrences of p, separated by op. 
-- Returns a value produced by a RIGHT associative application of all functions returned by op.
chainr1 :: Parser r a -> Parser r (a -> a -> a) -> Parser r a
p `chainr1` op = p >>= rest
  where rest a = (do f <- op
                     b <- p >>= rest
                     rest (f a b))
                 <|> return a

chain1 :: Parser r (a -> a) -> Parser r (a -> a)
chain1 p = p >>= rest
    where
        rest f0 = (do
            f <- p
            rest $ f . f0)
            <|> return f0

chain0 :: Parser r (a -> a) -> Parser r (a -> a)
chain0 p = chain1 p <|> return id


-- (count n p) sequentially parses p n-times
count :: Int -> Parser r a -> Parser r [a]
count 0 p = return []
count n p = (:) <$> p <*> count (n-1) p

-- apply parsers from list in a cyclic manner
cyclic :: [Parser r a] -> Parser r [a]
cyclic ps = mconcat <$> (many $ sequence ps)


--  combines all parsers in the specified list.
choice :: [Parser r a] -> Parser r a
choice []     = failure
choice [p]    = p
choice (p:ps) = p +++ choice ps

diff :: Parser r a -> Parser r String
diff p = do
    l1 <- look
    _ <- p
    l2 <- look
    let delta = length l1 - length l2
    return $ take delta l1


-------------------------------------------------------------------------------
-- Parsers
-------------------------------------------------------------------------------

spaces :: Parser r String
spaces = many $ oneOf " \n\r"

digit :: Parser r Char
digit = satisfy isDigit

alpha :: Parser r Char
alpha = satisfy isAlpha

name :: Parser r String
name = do
    c <- alpha 
    s <- many (alpha <|> digit)
    return (c:s)

alphaNum :: Parser r String
alphaNum = many (alpha <|> digit)

eof :: Parser r ()
eof = do
    s <- look
    if null s then return () else failure

-- (token p) parses p, discarding any whitespaces before and after it
token :: Parser r a -> Parser r a
token p = spaces *> p <* spaces

reserved :: String -> Parser r String
reserved s = token (string s)

char :: Char -> Parser r Char
char c = satisfy (c ==)

string :: String -> Parser r String
string [] = return []
string (c:cs) = do { char c; string cs; return (c:cs)}

parens :: String -> String -> Parser r a -> Parser r a
parens start end pa = (reserved start) *> pa <* (reserved end)

class Parsable a where
    parse :: Parser r a


instance Parsable Double where
    parse = floating

instance Parsable Integer where
    parse = integer

instance Parsable Bool where
    parse = boolean


integer :: Parser r Integer
integer = do
  s <- string "-" <|> return ""
  cs <- some digit
  return $ read (s ++ cs)

floating :: Parser r Double
floating = do
    s <- string "-" <|> return ""
    whole <- some digit
    frac <- (string ".") <++> (some digit) <|> return ""
    return $ read (s ++ whole ++ frac)

boolean :: Parser r Bool
boolean = t +++ f
    where t = (const True) <$> (string "1" +++ string "True" +++ string "true")
          f = (const False) <$> (string "0" +++ string "False" +++ string "false")