module Types where

data PredicateArgs = StringArg String | IntArg Int | PredicateArg Predicate
                   deriving (Show, Eq)

data Predicate = Predicate { predicateAlias :: Maybe String,
                             predicateNegated :: Bool,
                             predicateName :: String,
                             predicateArgs :: Maybe [PredicateArgs]
                           } deriving (Show, Eq)

data Type = Str | N | P [String]
          deriving (Show, Eq)

data TypeDef = TypeDef { typeName :: String,
                         typeArgs :: [Type]
                       } deriving (Show, Eq)

type Clause = [Predicate]
data Rule = Rule { premises :: [Clause] ,
                   consequences :: [Clause]
                 } deriving (Show, Eq)

