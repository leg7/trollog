module Types where

data Predicate = Predicate { predicateAlias :: Maybe String,
                             predicateNegated :: Bool,
                             predicateName :: String,
                             predicateArgs :: Maybe [String]
                           } deriving Show

data Type = Str | N | P [String]
          deriving Show

data TypeDef = TypeDef { typeName :: String,
                         typeArgs :: [Type]
                       } deriving Show

type Clause = [Predicate]
data Rule = Rule { premises :: [Clause] ,
                   consequences :: [Clause]
                 } deriving Show
