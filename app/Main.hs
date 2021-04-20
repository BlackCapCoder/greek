module Main where

import Parser
import AST


main :: IO ()
main = do
  parseFromFile (unparser parser) "code" >>= \case
    Nothing -> putStrLn "Parse error"
    Just a  -> mapM_ print a

parser :: Parser [TLD]
parser = do
  many spc
  many (tld <* end) <* eof

