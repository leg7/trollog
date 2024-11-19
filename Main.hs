module Main where
import Parser
import Engine
import Types
import Data.List ((\\))
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
  go facts declaredTypes rules
  where
    go :: [Predicate] -> [TypeDef] -> [Rule] -> IO ()
    go fcts ts rls = do
      putStr prompt
      hFlush stdout
      exprStr <- getExpr
      case app expr exprStr of
           Left err -> printRed err >> loop
           Right (e, _) -> case e of
                                ExprPredicate p -> case addFact p fcts ts of
                                                        Left err -> printRed err >> loop
                                                        Right newFcts -> go newFcts ts rls
                                ExprTypeDef t -> case addDeclaredTypes t ts of
                                                      Left err -> printRed err >> loop
                                                      Right ts' -> go fcts ts' rls
                                ExprRule r -> go fcts ts (r:rls)
                                ExprCommand c -> case c of
                                                      ShowFacts -> print fcts >> loop
                                                      -- ShowNonFacts -> print (filter (\x -> ))>> go fcts ts rls
                                                      ShowDeclaredTypes -> print ts >> loop
                                                      ShowRules -> print rls >> loop
                                                      ForwardChaining -> do printRed "Chaining..."
                                                                            let (newFcts, usedRules) = forwardChaining fcts rls ts
                                                                            printRed "Newly deduced facts: "
                                                                            print (newFcts \\ fcts)
                                                                            printRed "Rules used to deduce facts: "
                                                                            print usedRules
                                                                            go newFcts ts rls
                                                      Quit -> return ()
      where loop = go fcts ts rls
