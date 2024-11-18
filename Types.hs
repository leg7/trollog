module Types where

-- Predicate

data PredicateArgs = StringArg String | IntArg Int | PredicateArg Predicate
                   deriving (Show, Eq)

data Predicate = Predicate { predicateAlias :: Maybe String,
                             predicateNegated :: Bool,
                             predicateName :: String,
                             predicateArgs :: [PredicateArgs]
                           } deriving (Show, Eq)

instance Ord Predicate where
    compare p1 p2 = compare (predicateName p1) (predicateName p2)

emptyPredicate :: Predicate
emptyPredicate = Predicate {
  predicateAlias = Nothing,
  predicateNegated = False,
  predicateName = "",
  predicateArgs = []
}

-- Type

data Type = Str | N | P [String]
          deriving (Show, Eq)

data TypeDef = TypeDef { typeName :: String,
                         typeArgs :: [Type]
                       } deriving (Show, Eq)

emptyTypeDef :: TypeDef
emptyTypeDef = TypeDef {
  typeName = "",
  typeArgs = []
}

instance Ord TypeDef where
    compare td1 td2 = compare (typeName td1) (typeName td2)

-- Rules

type Conjunction = [Predicate]

data Rule = Rule { premises :: Conjunction,
                   consequences :: Conjunction
                 } deriving (Show, Eq)

-- Expr

data Expr = ExprPredicate Predicate
          | ExprTypeDef TypeDef
          | ExprRule Rule
          | ExprCommand Command
          deriving (Show, Eq)


-- Commands

data Command = ShowFacts | ShowNonFacts | ShowDeclaredTypes | ShowRules | Quit
             deriving (Eq, Show)

-- Key

class Key a where
  key ::  a -> String

instance Key Predicate where
  key = predicateName

instance Key TypeDef where
  key = typeName
