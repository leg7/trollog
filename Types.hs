module Types where

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


--un type peut etre un autre typedef (What the fuck jsp comment faire pour le type)
{-
        Predicate {
            predicateAlias = Nothing ,
            predicateNegated = False ,
            predicateName = "duoPokemon",
            predicateArgs = [
                (Predicate{
                    predicateAlias = Nothing ,
                    predicateNegated = False ,
                    predicateName = "pokemon",
                    predicateArgs = ["pikachu"]
                    }),
                (Predicate{
                    predicateAlias = Nothing ,
                    predicateNegated = False ,
                    predicateName = "pokemon",
                    predicateArgs = ["mewtwo"]
                    })
                ]
            }
-}

data Type = Str | N | P [String]
          deriving (Show, Eq)

data TypeDef = TypeDef { typeName :: String,
                         typeArgs :: [Type]
                       } deriving (Show, Eq)

instance Ord TypeDef where
    compare td1 td2 = compare (typeName td1) (typeName td2)


type Conjunction = [Predicate]

data Rule = Rule { premises :: Conjunction,
                   consequences :: Conjunction
                 } deriving (Show, Eq)


class Key a where
  key ::  a -> String

instance Key Predicate where
  key = predicateName

instance Key TypeDef where
  key = typeName
