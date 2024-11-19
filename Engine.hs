module Engine where
import Types
import Data.List ((\\), find)

-- Types

declaredTypes :: [TypeDef]
declaredTypes = []

declared :: TypeDef -> [TypeDef] -> Bool
declared t ts = case find (\x -> typeName t == typeName x) ts of
                     Nothing -> False
                     Just _ -> True

isPType :: Type -> Bool
isPType (P _) = True
isPType _ = False

pTypeVal :: Type -> [String]
pTypeVal (P n) = n
pTypeVal _ = error "I'll deal with this later"

typeDefsFromTypeArgs :: [Type] -> [TypeDef]
typeDefsFromTypeArgs =
  map (\x -> emptyTypeDef { typeName = x})
  . concatMap pTypeVal
  . filter isPType

addDeclaredTypes :: TypeDef -> [TypeDef] -> Either String [TypeDef]
addDeclaredTypes t ts
  | declared t ts = Left "A type with the same name has already been declared"
  | otherwise = let stypes = typeDefsFromTypeArgs (typeArgs t)
                    newTypes = filter (\x -> not $ declared x ts) stypes
                    in Right $ t:(newTypes ++ ts)

findOriginal :: Predicate -> [Predicate] -> Maybe Predicate
findOriginal p = find (\x -> case predicateAlias x of
                                  Nothing -> False
                                  Just a -> a == predicateName p)


wellTyped :: Predicate -> [TypeDef] -> [Predicate] -> Bool
wellTyped p tdfs fcts =
  case find (\x -> typeName x == predicateName p) tdfs of
       Nothing -> False
       Just t -> let pa = predicateArgs p
                     ta = typeArgs t
                     in (length pa == length ta) && and (zipWith cmp pa ta)
                       where
                         cmp (StringArg _) Str = True
                         cmp (IntArg _) N = True
                         -- This is disgusting, I'll refactor it later (i.e never)
                         cmp (PredicateArg p') (P pns) = if null (predicateArgs p')
                                                            then case findOriginal p' fcts of
                                                                      Nothing -> (predicateName p' `elem` pns) && wellTyped p' tdfs fcts
                                                                      Just o -> (predicateName o `elem` pns) && wellTyped o tdfs fcts
                                                            else (predicateName p' `elem` pns) && wellTyped p' tdfs fcts

                         cmp _ _ = False

-- Facts

facts :: [Predicate]
facts = []

nameTaken :: Predicate -> [Predicate] -> Bool
nameTaken p fcts = case find (\x -> predicateName x == predicateName p || predicateAlias x /= Nothing && predicateAlias x == predicateAlias p) fcts of
                            Nothing -> False
                            Just _ -> True

contradiction :: Predicate -> [Predicate] -> Bool
contradiction p fcts =
  case find (\x -> predicateName x == predicateName p && predicateNegated x /= predicateNegated p && predicateArgs x == predicateArgs p) fcts of
       Nothing -> False
       _ -> True

-- TODO: If fact is aliased add it's alias as a fact that takes 0 args with the same pred name
addFact :: Predicate -> [Predicate] -> [TypeDef] -> Either String [Predicate]
addFact p fcts ts
  | contradiction p fcts = Left "Fact contradicts another in the knowledge base"
  | not $ wellTyped p ts fcts = if null (predicateArgs p) && not (nameTaken p fcts) && p `notElem` fcts
                                   then Right (p:fcts)
                                   else Left "Fact doesn't have a matching type definition or name already taken"
  | p `elem` fcts = Right fcts
  | otherwise = case predicateAlias p of
                     Nothing -> Right (p:fcts)
                     Just a -> let sameContent = find (\x -> x == p { predicateAlias = Nothing }) $ map (\x -> x { predicateAlias = Nothing }) fcts
                                   sameAlias = find (\x -> predicateAlias x == Just a || predicateName x == a) fcts
                                   in case (sameContent, sameAlias) of
                                           (Nothing, Nothing) -> Right (emptyPredicate { predicateName = a } : p : fcts)
                                           _ -> Left "Fact already declared without an alias or with another alias"

addFacts :: [Predicate] -> [Predicate] -> [TypeDef] -> Either String [Predicate]
addFacts [] new _ = Right new
addFacts (f:fs) old ts = do
  case addFact f old ts of
       Left _ -> addFacts fs old ts
       Right new -> addFacts fs new ts

-- Rules

rules :: [Rule]
rules = []

-- Chaining

conflictSet :: [Predicate] -> [Rule] -> [Rule]
conflictSet [] rl = rl
conflictSet _ [] = []
conflictSet f rl = filter (all (`elem` f) . premises) rl

type Strategy = [Rule] -> Maybe Rule
liftConflictFirst :: Strategy
liftConflictFirst [] = Nothing
liftConflictFirst (r:_) = Just r

liftConflictRecent :: Strategy
liftConflictRecent = liftConflictFirst

forwardChaining :: [Predicate] -> [Rule] -> [TypeDef] -> Strategy -> ([Predicate], [Rule])
forwardChaining [] _ _ _ = ([],[])
forwardChaining f [] _ _ = (f,[])
forwardChaining f rl ts strat =
  go f (conflictSet f rl) [] strat
  where
    go :: [Predicate] -> [Rule] -> [Rule] -> Strategy -> ([Predicate], [Rule])
    go f' cs usedRules strat = case strat cs of
                                    Nothing -> (f', usedRules)
                                    Just r -> let nf = case addFacts (consequences r) f' ts of
                                                            Right nf' -> nf'
                                                            Left _ -> error "impossible"
                                                  usedRules' = r:usedRules
                                                  in go nf (conflictSet nf (rl \\ usedRules')) usedRules' strat


isFact :: Predicate -> [Predicate] -> Bool
isFact _ [] = False
isFact predi (h:t) = (predi==h) || (isFact predi t)

areFacts :: Conjunction -> [Predicate] -> Bool
areFacts [] _ = error "Conjunction can't be empty"
areFacts [predi] f = isFact predi f
areFacts (h:t) f = (isFact h f) && (areFacts t f)

matchingRules :: Conjunction -> [Rule] -> [Rule]
matchingRules [] _ = error "Conjunction can't be empty"
matchingRules _ [] = []
matchingRules conseq (h:t)=
  if (conseq == (consequences h))
     then h:(matchingRules conseq t)
     else matchingRules conseq t

premiseIsConsequence :: Conjunction -> [Rule] -> Bool
premiseIsConsequence [] _ = error "Can't have an empty premise"
premiseIsConsequence premis [] = False
premiseIsConsequence premis (h:t) = (premis == (consequences h)) || (premiseIsConsequence premis t)

applicableRules :: [Rule] -> [Rule] -> [Predicate] -> [Rule]
applicableRules [] _  _ = []
applicableRules (h:t) rs f =
  if (premiseIsConsequence (premises h) rs) || (areFacts (premises h) f)
     then h:(applicableRules t rs f)
     else applicableRules t rs f

getPremises :: [Rule] -> [Conjunction]
getPremises [] = []
getPremises (h:t) = (premises h):(getPremises t)

hasContradiction :: Conjunction -> [Predicate] -> Bool
hasContradiction [] f = error "Conjunction can't be empty"
hasContradiction [predi] f = contradiction predi f
hasContradiction (h:t) f = (contradiction h f) || (hasContradiction t f)

applyBCtoPremises :: (Conjunction -> [Predicate] -> [Rule] -> Bool) -> [Conjunction] -> [Predicate] -> [Rule] -> Bool
applyBCtoPremises bC [] f rs = False
applyBCtoPremises bC (h:t) f rs = (bC h f rs) || (applyBCtoPremises bC t f rs)

backwardChaining :: Conjunction -> [Predicate] -> [Rule] -> Bool
backwardChaining quest f rs
  | (hasContradiction quest f) = False
  | (areFacts quest f) = True
  | otherwise = let stack =  getPremises (applicableRules (matchingRules quest rs) rs f)
                in applyBCtoPremises backwardChaining stack f rs




{-backwardChainingWithTrace :: Conjunction -> [Predicate] -> [Rule] -> [Rule] -> (Bool,[Rule])
backwardChainingWithTrace quest f rs trace =
  if (areFacts quest f)
     then (True,trace)
     else
        let stack =  getPremises (applicableRules (matchingRules quest rs) rs f)
        in if stack!=[]
              then let firedRule = first (applicableRules (matchingRules quest rs))
     in
     applyBCtoPremises backwardChaining stack f rs-}






--Test

--Daniel
fnon1 = emptyPredicate { predicateName = "A" ,predicateNegated = True}
f1 = emptyPredicate { predicateName = "A" }
f2 = emptyPredicate { predicateName = "B" }
f3 = emptyPredicate { predicateName = "C" }
f4 = emptyPredicate { predicateName = "D" }
f5 = emptyPredicate { predicateName = "E" }

rul1 :: Rule
rul1 = Rule {
    premises = [f1] ,
    consequences = [fnon1]
}

rul2 :: Rule
rul2 = Rule {
  premises = [f1],
  consequences = [f3]
}

rul3 :: Rule
rul3 = Rule {
  premises = [f3],
  consequences = [f4]
}

rls = [rul1]
fts = [f1]
question = [fnon1]


--Leonard
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
