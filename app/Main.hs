module Main where


import Play
import Parser
import Text.Trifecta


main :: IO ()
main = do

  Just prog <- parseFromFile (toParser parser) "code"

  mapM_ print prog

