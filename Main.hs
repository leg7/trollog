module Main where
import Parser
import System.IO (stdout, hFlush)

-- TODO: Read until you encounter a . (Multiline editing)
main :: IO ()
main = do putStr "🧢  >>= "
          hFlush stdout
          parseExpr
          main
