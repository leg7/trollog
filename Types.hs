module Types where

data PredicateArgs = StringArg String | IntArg Int | PredicateArg Predicate
                   deriving (Show, Eq)


data Predicate = Predicate { predicateAlias :: Maybe String,
                             predicateNegated :: Bool,
                             predicateName :: String,
                             predicateArgs :: [PredicateArgs]
                           } deriving (Show, Eq)

emptyPredicate :: Predicate
emptyPredicate = Predicate {
  predicateAlias = Nothing,
  predicateNegated = False,
  predicateName = "",
  predicateArgs = []
}

data Type = Str | N | P [String]
          deriving (Show, Eq)

data TypeDef = TypeDef { typeName :: String,
                         typeArgs :: [Type]
                       } deriving (Show, Eq)

type Clause = [Predicate]
data Rule = Rule { premises :: [Clause] ,
                   consequences :: [Clause]
                 } deriving (Show, Eq)

