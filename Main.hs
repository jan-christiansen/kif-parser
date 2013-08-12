module Main where

import System.Environment (getArgs)

import Text.KIF
import Text.KIF.Parser

main :: IO ()
main = do
  args <- getArgs
  case args of
       ["-o", "junit", filePath] -> do
                                 kifTest <- readKIF filePath
                                 putStrLn (toJUnit kifTest)
       ["-o", "markdown", filePath] -> do 
                                 kifTest <- readKIF filePath
                                 putStrLn (toMarkdown kifTest)
       args -> print args >> putStrLn "Usage : kif-parser -o <junit|markdown> kif-output-file"
