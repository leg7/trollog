module Engine where
import Types
import Data.List (delete, (\\))

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

bstHasKey :: (Key a) => BST a -> String -> Bool
bstHasKey Empty _ = False
bstHasKey (Node x l r) k
  | k > kx = bstHasKey r k
  | k < kx = bstHasKey l k
  | otherwise = True
  where kx = key x

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

-- TODO: getElement and check if args are the same type
wellTyped :: Predicate -> BST TypeDef -> Bool
wellTyped p t = bstHasKey t (key p)

contradiction :: Predicate -> BST Predicate -> Bool
contradiction p nf = bstHasKey nf (key p)


--Chainage Avant
{-chainageAvant :: Predicate -> Bool
chainageAvant predi =
    if (isPredicate predi) then true
    else -}

--Test

conflictSet :: [Predicate] -> [Rule] -> [Rule]
conflictSet [] rl = rl
conflictSet _ [] = []
conflictSet f rl = filter (all (`elem` f) . premises) rl

liftConflict :: [Rule] -> Maybe Rule
liftConflict [] = Nothing
liftConflict (r:_) = Just r

forwardChaining :: [Predicate] -> [Rule] -> [Predicate]
forwardChaining [] _ = []
forwardChaining f [] = f
forwardChaining f rl =
  go f (conflictSet f rl) []
  where
    go :: [Predicate] -> [Rule] -> [Rule] -> [Predicate]
    go f' cs usedRules = case liftConflict cs of
                             Nothing -> f'
                             Just r -> let nf = consequences r ++ f'
                                           usedRules' = r:usedRules
                                           in go nf (conflictSet nf (rl \\ usedRules')) usedRules'

pa :: Predicate
pa = emptyPredicate { predicateName = "A" }

pb :: Predicate
pb = emptyPredicate { predicateName = "B" }

pc :: Predicate
pc = emptyPredicate { predicateName = "C" }

-- :)
pd :: Predicate
pd = emptyPredicate { predicateName = "D" }

pe :: Predicate
pe = emptyPredicate { predicateName = "E" }


r1 :: Rule
r1 = Rule {
    premises = [pa] ,
    consequences = [pc]
}

r2 :: Rule
r2 = Rule {
  premises = [pa, pb],
  consequences = [pd]
}

r3 :: Rule
r3 = Rule {
  premises = [pa, pc, pd],
  consequences = [pe]
}

-- r1 :: Rule
-- r1 = Rule {
--     premises = [pa] ,
--     consequences = [pc]
-- }
--
-- r2 :: Rule
-- r2 = Rule {
--   premises = [pc],
--   consequences = [pd]
-- }
--
-- r3 :: Rule
-- r3 = Rule {
--   premises = [pd],
--   consequences = [pe]
-- }


fs :: [Predicate]
fs = [pa, pb]

rll :: [Rule]
rll = [r1, r2, r3]

-- conflictSet fs rll
-- liftConflict $ conflictSet fs rll
-- --
-- conflictSet (pc:fs) (delete r1 rll)
-- liftConflict $ conflictSet (pc:fs) (delete r1 rll)
-- --
-- conflictSet (pd:pc:fs) (delete r2 (delete r1 rll))
-- liftConflict $ conflictSet (pd:pc:fs) (delete r2 (delete r1 rll))

-- forwardChaining fs rll

{- bstGetElement fs "A"
bstGetElement fs "B"
bstGetElement fs "C"
bstGetElement fs "D" -}

nfs :: BST Predicate
nfs = bstAddElement nonFacts pc

-- contradiction pb nfs
