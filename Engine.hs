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

bstGetElement :: (Key a) => BST a -> String -> Maybe a
bstGetElement Empty _ = Nothing
bstGetElement (Node x l r) k
  | k > kx = bstGetElement r k
  | k < kx = bstGetElement l k
  | otherwise = Just x
  where kx = key x


bstToList :: (Ord a) => BST a -> [a]
bstToList Empty = []
bstToList (Node a left right) = a:(bstToList left)++(bstToList right)


--Dictionnary
type Dict a b = [(a,b)]

dictAddKV :: Dict a b -> a -> b -> Dict a b
dictAddKV [] k value = [(k,value)]
dictAddKV (_:t) k value = dictAddKV t k value

dictGetValue :: (Eq a) => Dict a b -> a -> Maybe b
dictGetValue [] _ = Nothing
dictGetValue (h:t) k =
  if k == fst h then Just (snd h)
                else dictGetValue t k

dictGetKey :: (Eq b) => Dict a b -> b -> Maybe a
dictGetKey [] _ = Nothing
dictGetKey (h:t) value =
  if value == snd h then Just (fst h)
                    else dictGetKey t value

declaredTypes :: BST TypeDef
declaredTypes = Empty

facts :: BST Predicate
facts = Empty

nonFacts :: BST Predicate
nonFacts = Empty

rules :: Dict [Predicate] [Predicate]
rules = []
addRule :: Dict [Predicate] [Predicate] -> Rule -> Dict [Predicate] [Predicate]
addRule rs r = dictAddKV rs (premises r) (consequences r)

-- wellTyped :: Predicate -> Bool
-- wellTyped p = bstHasElement declaredTypes p

--Chainage Avant
{-chainageAvant :: Predicate -> Bool
chainageAvant predi =
    if (isPredicate predi) then true
    else -}

--Test

pa :: Predicate
pa = emptyPredicate { predicateName = "a" }

pb :: Predicate
pb = emptyPredicate { predicateName = "B" }

pc :: Predicate
pc = emptyPredicate { predicateName = "C" }

-- :)
pd :: Predicate
pd = emptyPredicate { predicateName = "D" }

rule :: Rule
rule = Rule {
    premises = [pa, pb] ,
    consequences = [pc, pd]
}

fs :: BST Predicate
fs = bstAddElement (bstAddElement (bstAddElement (bstAddElement facts pb) pc) pc) pd

{- bstGetElement fs "A"
bstGetElement fs "B"
bstGetElement fs "C"
bstGetElement fs "D" -}
