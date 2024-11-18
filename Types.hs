module Types where

-- Predicate

data PredicateArgs = StringArg String | IntArg Int | PredicateArg Predicate
                   deriving (Eq)

instance Show PredicateArgs where
    show (IntArg i) = "IntArg " ++ show i
    show (PredicateArg p) = "\nPredicateArg " ++ show p
    show (StringArg a) = "StringArg " ++ show a

data Predicate = Predicate { predicateAlias :: Maybe String,
                             predicateNegated :: Bool,
                             predicateName :: String,
                             predicateArgs :: [PredicateArgs]
                           } deriving (Eq)

instance Show Predicate where
    show (Predicate alias negated name args) =
        "Predicate {\n" ++
        "  Alias: " ++ showAlias alias ++ ",\n" ++
        "  Negated: " ++ show negated ++ ",\n" ++
        "  Name: " ++ name ++ ",\n" ++
        "  Args: [" ++ showArgs args ++ "]\n" ++
        "}"

showAlias :: Maybe String -> String
showAlias Nothing  = "None"
showAlias (Just a) = a

showArgs :: [PredicateArgs] -> String
showArgs [] = ""
showArgs [arg] = "  " ++ show arg
showArgs (arg:args) = "  " ++ show arg ++ ",\n" ++ showArgs args

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
    show N         = "N"
    show (P args)  = "P[" ++ unwords args ++ "]"

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
        "TypeDef {\n" ++
        "  typeName: " ++ name ++ ",\n" ++
        "  typeArgs: [" ++ showTypeArgs args ++ "]\n" ++
        "}\n"

-- Helper function to format the type arguments
showTypeArgs :: [Type] -> String
showTypeArgs [] = ""
showTypeArgs [t] = show t
showTypeArgs (t:ts) = show t ++ ", " ++ showTypeArgs ts

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
