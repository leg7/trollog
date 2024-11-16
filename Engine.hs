module Engine where
import Types

-- or a tree
newtype HashMap a = HashMap [(Int, a)]

declaredTypes :: HashMap TypeDef
declaredTypes = undefined

facts :: HashMap Predicate
facts = undefined

nonFacts :: HashMap Predicate
nonFacts = undefined

rules :: HashMap Rule
rules = undefined

--

addPredicate :: HashMap Predicate -> HashMap Predicate
addPredicate = undefined

addRule :: HashMap Rule -> HashMap Rule
addRule = undefined

addTypeDef :: HashMap TypeDef ->  HashMap TypeDef
addTypeDef = undefined

getPredicate :: HashMap Predicate -> Predicate
getPredicate = undefined


-- Chainage avant, arr, + pile

