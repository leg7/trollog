module Engine where
import Types

--Binary Search Tree
data BST a = Empty | Node a (BST a) (BST a) deriving (Show)

bstAddElement :: (Ord a) => BST a -> a -> BST a
bstAddElement Empty a = Node a Empty Empty
bstAddElement (Node b left right) a =
    if (a > b) then (Node b left (bstAddElement right a))
               else (Node b (bstAddElement left a) right)

bstHasElement :: (Ord a) => BST a -> a -> Bool
bstHasElement Empty _ = False
bstHasElement (Node b left right) a =
    if (a==b) then True
              else if (a > b) then bstHasElement right a
                              else bstHasElement left a

bstToList :: (Ord a) => BST a -> [a]
bstToList Empty = []
bstToList (Node a left right) = a:(bstToList left)++(bstToList right)




--Dictionnary
type Dict a b = [(a,b)]

dictAddKV :: Dict a b -> a -> b -> Dict a b
dictAddKV [] key value = [(key,value)]
dictAddKV (_:t) key value = dictAddKV t key value

dictGetValue :: (Eq a) => Dict a b -> a -> Maybe b
dictGetValue [] _ = Nothing
dictGetValue (h:t) key =
    if (key == (fst h)) then Just (snd h)
                           else dictGetValue t key

dictGetKey :: (Eq b) => Dict a b -> b -> Maybe a
dictGetKey [] _ = Nothing
dictGetKey (h:t) value =
    if (value == (snd h)) then Just (fst h)
                             else dictGetKey t value







--DECLARED TYPES
declaredTypes :: BST TypeDef
declaredTypes = Empty

addTypeDef :: BST TypeDef -> TypeDef -> BST TypeDef
addTypeDef dT td = bstAddElement dT td

typeDefExists :: BST TypeDef -> TypeDef -> Bool
typeDefExists dT td = bstHasElement dT td




--FACTS
facts :: BST Predicate
facts = Empty

addPredicate :: BST Predicate -> Predicate -> BST Predicate
addPredicate f predi = bstAddElement f predi

isPredicate :: BST Predicate -> Predicate -> Bool
isPredicate f predi = bstHasElement f predi




--NONFACTS
nonFacts :: BST Predicate
nonFacts = Empty

addNonPredicate :: BST Predicate -> Predicate -> BST Predicate
addNonPredicate nF nonPredi = bstAddElement nF nonPredi

isNonPredicate :: BST Predicate -> Predicate -> Bool
isNonPredicate nF nonPredi = bstHasElement nF nonPredi




--RULES
rules :: Dict [Predicate] [Predicate]
rules = []
addRule :: Dict [Predicate] [Predicate] -> Rule -> Dict [Predicate] [Predicate]
addRule rs r = dictAddKV rs (premises r) (consequences r)





--Chainage Avant
{-chainageAvant :: Predicate -> Bool
chainageAvant predi =
    if (isPredicate predi) then true
    else -}









--Test

predi1 :: Predicate
predi1 = Predicate {
        predicateAlias = Nothing ,
        predicateNegated = True ,
        predicateName = "A",
        predicateArgs = []
    }

predi2 :: Predicate
predi2 = Predicate {
        predicateAlias = Nothing ,
        predicateNegated = True ,
        predicateName = "B",
        predicateArgs = []
    }

rule :: Rule
rule = Rule {
    premises = [predi1] ,
    consequences = [predi2]
}







