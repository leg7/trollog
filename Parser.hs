{- HLINT ignore "Use lambda-case" -}
module Parser where
import Data.Char
import Control.Applicative (Alternative, (<|>), empty)
import Distribution.Utils.Generic (isAsciiAlphaNum)

newtype Parser a = S (String -> Maybe (a, String))


app :: Parser a -> (String -> Maybe (a, String))
app (S f) = f

item :: Parser Char
item = S (\inp -> case inp of
                       c:inp' -> Just (c, inp')
                       [] -> Nothing)

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = S (\inp -> case app p inp of
                             Just (c, inp') -> Just (g c, inp')
                             Nothing -> Nothing)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = S (\inp -> Just (x, inp))
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa     = S (\inp -> case app pf inp of
                                  Just (f, inp') -> app (f <$> pa) inp'
                                  Nothing -> Nothing)

instance Alternative Parser where
  empty :: Parser a
  empty = S (const Nothing)
  (<|>) :: Parser a -> Parser a -> Parser a
  pl <|> pr = S (\inp -> case app pl inp of
                              Nothing -> app pr inp
                              success -> success)

instance Monad Parser where
  return :: a -> Parser a
  return = pure
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = S (\inp -> case app p inp of
                            Nothing -> Nothing
                            Just (result, inp') -> app (f result) inp')


sat :: (Char -> Bool) -> Parser Char
sat p = S (\inp -> case inp of
                        [] -> Nothing
                        c:inp' -> if p c then Just (c, inp') else Nothing)

char :: Char -> Parser Char
char c = sat (== c)

lower :: Parser Char
lower = sat isAsciiLower

upper :: Parser Char
upper = sat isAsciiUpper

alphaNumeric :: Parser Char
alphaNumeric = sat isAsciiAlphaNum

digit :: Parser Char
digit = sat isDigit

many :: Parser Char -> Parser String
many p = do c <- p
            cs <- many p
            return (c:cs)
          <|>
            return []

some :: Parser Char -> Parser String
some p = do c <- p
            return [c]
          <|>
            return []

spaces :: Parser String
spaces = many $ sat isSpace


identifier :: Parser String
identifier = do c <- lower
                cs <- many (alphaNumeric <|> char '_')
                return (c:cs)

argument :: Parser String
argument = do identifier
           <|>
           do strBegin <- char '"'
              str <- many (sat (/= '"'))
              strEnd <- char '"'
              return ((strBegin:str) ++ [strEnd])
           <|>
           do many digit


argumentList :: Parser [String]
argumentList = do a <- argument
                  do spaces
                     char ','
                     spaces
                     as <- argumentList
                     return (a:as)
                    <|>
                     return [a]

data Predicate = Predicate { negated :: Bool
                           , name :: String
                           , args :: [String]
                           } deriving Show

predicate :: Parser Predicate
predicate = do char '\\'
               p <- predicate'
               return p { negated = True }
            <|>
            do predicate'
  where predicate' = do spaces
                        name <- identifier
                        spaces
                        char '('
                        spaces
                        args <- argumentList
                        return Predicate { negated = False, name, args }

type Alias = (String, Predicate)

alias :: Parser Alias
alias = do a <- identifier
           spaces
           char '='
           spaces
           p <- predicate
           return (a, p)

expr :: Parser Bool
expr = do alias
          return True
      <|>
       do predicate
          return True

app predicate "Pokemon(x)"

parseExpr :: IO ()
parseExpr = do inp <- getLine
               case app expr inp of
                    Just (r,_) -> print r
                    Nothing -> print "Couldn't parse expr"
