module Main where
import Parser
import Engine
import Types
import System.IO (stdout, hFlush)

-- TODO: Read until you encounter a . (Multiline editing)
prompt :: String
prompt = "🧢  \x1b[31m>>\x1b[0m\x1b[90m=\x1b[0m\x1b[97m=\x1b[0m "

printRed :: String -> IO ()
printRed str = putStrLn "\x1b[31m" >> putStrLn str >> putStrLn "\x1b[0m"

getExpr :: IO String
getExpr = go []
  where
    go :: String ->  IO String
    go input = do
      c <- getChar
      let rstr = c:input
      if c == '.' then return (reverse rstr) else go rstr

main :: IO ()
main =
  go facts nonFacts declaredTypes rules
  where
    go :: BST Predicate -> BST Predicate -> BST TypeDef -> Dict Conjunction Conjunction -> IO ()
    go f nf ty rl = do
      putStr prompt
      hFlush stdout
      exprStr <- getExpr
      case app expr exprStr of
           Left err -> printRed err
           Right (e, _) -> case e of
                                ExprPredicate p -> if predicateNegated p
                                                      then go f (bstAddElement nf p) ty rl
                                                      else go (bstAddElement f p) nf ty rl
                                ExprTypeDef t -> go f nf (bstAddElement ty t) rl
                                ExprRule r -> go f nf ty (dictAddKV rl (premises r) (consequences r))
                                ExprCommand c -> case c of
                                                      ShowFacts -> print f >> go f nf ty rl
                                                      ShowNonFacts -> print nf >> go f nf ty rl
                                                      ShowDeclaredTypes -> print ty >> go f nf ty rl
                                                      ShowRules -> print rl >> go f nf ty rl
                                                      Quit -> return ()
