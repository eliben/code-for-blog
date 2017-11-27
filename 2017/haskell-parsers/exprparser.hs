-- Basic parser with applicative and monadic combinators.
--
-- Based upon chapter 13 of Hutton's "Programming in Haskell" (2nd ed)
--
-- Eli Bendersky [http://eli.thegreenplace.net]
-- This code is in the public domain.
import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
                          []        -> []
                          [(v,out)] -> [(g v,out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v,inp)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                            []        -> []
                            [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
  -- return :: a -> Parser a
  return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                          []        -> []
                          [(v,out)] -> parse (f v) out)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp -> case parse p inp of
                          []        -> parse q inp
                          [(v,out)] -> [(v,out)])

-- Basic building block: item parses a single char from the input.
item :: Parser Char
item = P (\inp -> case inp of
                    []      -> []
                    (x:xs)  -> [(x,xs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

-- Token wraps a parser with space-ignoring capabilities.
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

-- BNF grammar for our language:
--
-- expr       ::= ifexpr | cmpexpr
-- ifexpr     ::= 'if' expr 'then' expr 'else' expr
-- cmpexpr    ::= sumexpr ('==' cmpexpr | eps)
-- sumexpr    ::= term ('+' sumexpr | eps)
-- term       ::= factor ('*' term | eps)
-- factor     ::= '(' expr ')' | <natural>

expr :: Parser Int
expr = ifexpr' <|> cmpexpr

cmpexpr :: Parser Int
cmpexpr = do se <- sumexpr
             do symbol "=="
                e <- cmpexpr
                return $ fromEnum (se == e)
              <|> return se

sumexpr :: Parser Int
sumexpr = do t <- term
             do symbol "+"
                se <- sumexpr
                return (t + se)
              <|> return t

term :: Parser Int
term = do f <- factor
          do symbol "*"
             t <- term
             return (f * t)
           <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
          <|> natural

-- Note: both the 'then' and 'else' clauses are evaluated eagerly; We can make
-- it lazy without too much effort, if needed.
ifexpr :: Parser Int
ifexpr = do symbol "if"
            cond <- expr
            symbol "then"
            thenExpr <- expr
            symbol "else"
            elseExpr <- expr
            return (if cond == 0 then elseExpr else thenExpr)

-- An applicative version of ifexpr.
ifexpr' :: Parser Int
ifexpr' =
  selector <$> symbol "if" <*> expr
           <*> symbol "then" <*> expr
           <*> symbol "else" <*> expr
  where selector _ cond _ t _ e = if cond == 0 then e else t
