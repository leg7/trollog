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

isFact :: Predicate -> [Predicate] -> Bool
isFact p fcts = case find (== p { predicateAlias = Nothing }) fcts of
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
  | not $ wellTyped p ts fcts = Left "Fact doesn't have a matching type definition" -- Check if it has 0 args
  | p `elem` fcts = Right fcts
  | otherwise = case predicateAlias p of
                     Nothing -> Right (p:fcts)
                     Just a -> let sameContent = find (\x -> x == p { predicateAlias = Nothing }) $ map (\x -> x { predicateAlias = Nothing }) fcts
                                   sameAlias = find (\x -> predicateAlias x == Just a) fcts
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

liftConflict :: [Rule] -> Maybe Rule
liftConflict [] = Nothing
liftConflict (r:_) = Just r

forwardChaining :: [Predicate] -> [Rule] -> [TypeDef] -> ([Predicate], [Rule])
forwardChaining [] _ _ = ([],[])
forwardChaining f [] _ = (f,[])
forwardChaining f rl ts =
  go f (conflictSet f rl) []
  where
    go :: [Predicate] -> [Rule] -> [Rule] -> ([Predicate], [Rule])
    go f' cs usedRules = case liftConflict cs of
                             Nothing -> (f', usedRules)
                             Just r -> let nf = case addFacts (consequences r) f' ts of
                                                     Right nf' -> nf'
                                                     Left _ -> error "impossible"
                                           usedRules' = r:usedRules
                                           in go nf (conflictSet nf (rl \\ usedRules')) usedRules'

--Test

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
