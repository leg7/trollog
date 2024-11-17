module Main where
import Parser
import System.IO (stdout, hFlush)

-- TODO: Read until you encounter a . (Multiline editing)
prompt :: String
prompt = "🧢  \x1b[31m>>\x1b[0m\x1b[90m=\x1b[0m\x1b[97m=\x1b[0m "

main :: IO ()
main = do putStr prompt
          hFlush stdout
          parseExpr
          main
