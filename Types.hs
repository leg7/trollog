module Types where
import Data.List (intersperse)

-- Predicate

data PredicateArgs = StringArg String | IntArg Int | PredicateArg Predicate
                   deriving (Eq)

instance Show PredicateArgs where
    show (IntArg i) = show i
    show (PredicateArg p) = show p
    show (StringArg a) = a

data Predicate = Predicate { predicateAlias :: Maybe String,
                             predicateNegated :: Bool,
                             predicateName :: String,
                             predicateArgs :: [PredicateArgs]
                           } deriving (Eq)

instance Show Predicate where
    show (Predicate alias negated name args) =
      let showNeg = if negated then "\\" else ""
          in showNeg ++ name ++ showAlias alias ++ "(" ++ myShowList args ++ ")"

showAlias :: Maybe String -> String
showAlias Nothing  = ""
showAlias (Just a) = "/" ++ a

myShowList :: Show a => [a] -> String
myShowList [] = ""
myShowList [arg] = show arg
myShowList (arg:args) = show arg ++ ", " ++ myShowList args

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
    deriving (Eq)

instance Show Type where
    show Str       = "Str"
    show N         = "Int"
    show (P args)  = unwords $ intersperse "|" args


data TypeDef = TypeDef { typeName :: String,
                         typeArgs :: [Type]
                       } deriving (Eq)

emptyTypeDef :: TypeDef
emptyTypeDef = TypeDef {
  typeName = "",
  typeArgs = []
}

instance Ord TypeDef where
    compare td1 td2 = compare (typeName td1) (typeName td2)

instance Show TypeDef where
    show (TypeDef name args) =
      name ++ "(" ++ myShowList args ++ ")"

-- Rules

type Conjunction = [Predicate]

data Rule = Rule { premises :: Conjunction,
                   consequences :: Conjunction
                 } deriving (Eq)

-- Show instance for Rule with proper formatting
instance Show Rule where
    show (Rule prem cons) =
        "Rule { " ++ myShowList prem ++ " -> " ++ myShowList cons ++ " }\n"

-- Expr

data Expr = ExprPredicate Predicate
          | ExprTypeDef TypeDef
          | ExprRule Rule
          | ExprCommand Command
          deriving (Show, Eq)


-- Commands

data Command = ShowFacts
             | ShowDeclaredTypes
             | ShowRules
             | ForwardChaining
             | StrategyRecent
             | StrategyFirst
             | Quit
             deriving (Eq, Show)

-- Key

class Key a where
  key ::  a -> String

instance Key Predicate where
  key = predicateName

instance Key TypeDef where
  key = typeName
