module Engine where
import Types
import Data.List (delete, (\\), find)

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

-- TODO: Add newTypes to facts
addDeclaredTypes :: TypeDef -> [TypeDef] -> Either String [TypeDef]
addDeclaredTypes t ts
  | declared t ts = Left "A type with the same name has already been declared"
  | otherwise = let stypes = typeDefsFromTypeArgs (typeArgs t)
                    newTypes = filter (\x -> not $ declared x ts) stypes
                    in Right $ t:(newTypes ++ ts)

typeNameOfPredAlias :: Predicate -> [TypeDef] -> Maybe String
typeNameOfPredAlias p ts = case predicateAlias p of
                                Nothing -> Nothing
                                Just a -> find (== a) $ map typeName ts

-- TODO: Support aliases
wellTyped :: Predicate -> [TypeDef] -> Bool
wellTyped p tdfs =
  case find (\x -> typeName x == predicateName p) tdfs of
       Nothing -> False
       Just t -> let pa = predicateArgs p
                     ta = typeArgs t
                     in (length pa == length ta) && and (zipWith cmp pa ta)
                       where
                         cmp (StringArg _) Str = True
                         cmp (IntArg _) N = True
                         cmp (PredicateArg p') (P pns) =
                           case typeNameOfPredAlias p' tdfs of
                                Nothing -> False
                                Just tName -> ((predicateName p' `elem` pns) || (tName `elem` pns)) && wellTyped p' tdfs
                         cmp _ _ = False


-- pp = [Predicate {predicateAlias = Just "a", predicateNegated = False, predicateName = "p", predicateArgs = [IntArg 2]},Predicate {predicateAlias = Nothing, predicateNegated = False, predicateName = "p", predicateArgs = [IntArg 1]}]
-- tt = [TypeDef {typeName = "y", typeArgs = [P ["p","test"]]},TypeDef {typeName = "test", typeArgs = []},TypeDef {typeName = "p", typeArgs = [N]}]
--
-- p = Predicate {
--   predicateAlias = Nothing,
--   predicateName = "y",
--   predicateNegated = False,
--   predicateArgs = [PredicateArg (
--     Predicate {
--       predicateAlias = Nothing,
--       predicateName = "p",
--       predicateNegated = False,
--       predicateArgs = [ IntArg 1 ]
--     }
--   )]
-- }
-- typeNameOfPred (head pp) tt
-- wellTyped p tt

-- Facts

facts :: [Predicate]
facts = []

contradiction :: Predicate -> [Predicate] -> Bool
contradiction p fcts =
  case find (\x -> predicateName x == predicateName p && predicateNegated x /= predicateNegated p) fcts of
                            Nothing -> False
                            _ -> True

addFact :: Predicate -> [Predicate] -> [TypeDef] -> Either String [Predicate]
addFact p fcts ts
  | contradiction p fcts = Left "Fact contradicts another in the knowledge base"
  | not $ wellTyped p ts = Left "Fact doesn't have a matching type definition" -- Check if it has 0 args
  | p `elem` fcts = Right fcts
  | otherwise = case predicateAlias p of
                     Nothing -> Right (p:fcts)
                     Just a -> case find (\x -> x == p { predicateAlias = Nothing }) fcts of
                                    Nothing -> Right (p:fcts)
                                    _ -> Left "Fact already declared without an alias"

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
