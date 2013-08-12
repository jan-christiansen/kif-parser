module Main where

import System.Environment (getArgs)
import System.Time
import Network.HostName

import Text.KIF
import Text.KIF.Parser

main :: IO ()
main = do
  args <- getArgs
  case args of
       ["-o", "junit", filePath] -> do
                                 kifTest <- readKIF filePath
                                 timeStamp <- getClockTime
                                 hostName <- getHostName
                                 putStrLn (toJUnit hostName timeStamp kifTest)
       ["-o", "markdown", filePath] -> do 
                                 kifTest <- readKIF filePath
                                 putStrLn (toMarkdown kifTest)
       _ -> putStrLn "Usage : kif-parser -o [junit|markdown] <kif-log-file>"
