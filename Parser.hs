{- HLINT ignore "Use lambda-case" -}
{- HLINT ignore "Use foldr" -}
module Parser where
import Types
import Data.Char
import Control.Applicative (Alternative, (<|>), empty, some, many)
import Distribution.Utils.Generic (isAsciiAlphaNum)

newtype Parser a = S { app :: String -> Either String (a, String) }

item :: Parser Char
item = S (\inp -> case inp of
                       c:inp' -> Right (c, inp')
                       [] -> Left "Couldn't parse an item")

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = S (\inp -> case app p inp of
                             Right (c, inp') -> Right (g c, inp')
                             Left errMsg -> Left errMsg)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = S (\inp -> Right (x, inp))
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa     = S (\inp -> case app pf inp of
                                  Right (f, inp') -> app (f <$> pa) inp'
                                  Left errMsg -> Left errMsg)

instance Alternative Parser where
  empty :: Parser a
  empty = S (\_ -> Left "")
  (<|>) :: Parser a -> Parser a -> Parser a
  pl <|> pr = S (\inp -> case app pl inp of
                              Left _ -> app pr inp
                              success -> success)

instance Monad Parser where
  return :: a -> Parser a
  return = pure
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = S (\inp -> case app p inp of
                            Left errMsg -> Left errMsg
                            Right (result, inp') -> app (f result) inp')


sat :: (Char -> Bool) -> Parser Char
sat p = S (\inp -> case inp of
                        [] -> Left "ParseError, sat: Couldn't parse empty string"
                        c:inp' -> if p c then Right (c, inp') else Left $ "Couldn't parse '" ++ inp' ++ "' after character\"" ++ [c] ++ "\"")

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

spaces :: Parser String
spaces = many $ sat isSpace

char' :: Char -> Parser String
char' c = do spaces
             char c
             spaces
             return [c]

string :: String -> Parser String
string [] = return []
string (x:xs) = (:) <$> char x <*> string xs

identifier :: Parser String
identifier = spaces *> fmap (:) lower <*> many (alphaNumeric <|> char '_') <* spaces

-- TODO: Try writing this in applicative style with `many`
list :: Parser a -> Char -> Parser [a]
list elemParser delim = do e <- elemParser
                           do char' delim
                              es <- list elemParser delim
                              return (e:es)
                              <|>
                              return [e]

-- End of Primitives

predicate :: Parser Predicate
predicate = do char' '\\'
               p <- predicate'
               return p { predicateNegated = True }
               <|>
               predicate'
  where predicate' :: Parser Predicate
        predicate' = do predicateName <- identifier
                        do char' '('
                           predicateArgs <- list argument ','
                           char' ')'
                           return Predicate { predicateAlias, predicateNegated, predicateName, predicateArgs }
                          <|>
                           return Predicate { predicateAlias, predicateNegated, predicateName, predicateArgs = [] }
                        where predicateNegated = False
                              predicateAlias = Nothing
        argument :: Parser PredicateArgs
        argument = do PredicateArg <$> predicate
                   <|>
                   do p <- identifier
                      return $ PredicateArg (emptyPredicate { predicateAlias = Just p })
                   <|>
                   do strBegin <- char' '"'
                      str <- some (sat (/= '"'))
                      strEnd <- char' '"'
                      return $ StringArg (strBegin ++ str ++ strEnd)
                   <|>
                   do numStr <- some digit
                      return $ IntArg $ read numStr

alias :: Parser Predicate
alias = do a <- identifier
           char' '='
           p <- predicate
           return p { predicateAlias = Just a }

typeDef :: Parser TypeDef
typeDef = do char' '!'
             typeName <- identifier
             char' '('
             typeArgs <- list type' ','
             char' ')'
             return TypeDef { typeName, typeArgs }
  where
    type' :: Parser Type
    type' = (string "String" >> return Str)
            <|> (string "Int" >> return N)
            <|> (P <$> list identifier '|')

rule :: Parser Rule
rule = do premises <- conjunction
          spaces
          string "->"
          spaces
          consequences <- conjunction
          return Rule { premises, consequences }
  where conjunction = list predicate ','

data Expr = ExprPredicate Predicate
          | ExprTypeDef TypeDef
          | ExprRule Rule
          deriving (Show, Eq)

expr :: Parser Expr
expr = do r <- rule
          char' '.'
          return $ ExprRule r
      <|>
       do t <- typeDef
          char' '.'
          return $ ExprTypeDef t
      <|>
       do p <- alias <|> predicate
          char' '.'
          return $ ExprPredicate p

printRed :: String -> IO ()
printRed str = putStrLn "\x1b[31m" >> putStrLn str >> putStrLn "\x1b[0m"

parseExpr :: IO ()
parseExpr = go []
  where go acc = do c <- getChar
                    let rstr = c:acc
                    if c == '.' then
                      case app expr $ reverse rstr of
                           Right (r,_) -> putStrLn (show r)
                           Left err -> printRed err
                    else
                      go rstr
