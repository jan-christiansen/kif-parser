module Main where

import System.Environment (getArgs)
import System.Time
import Network.HostName
import Text.Twine
import qualified Data.ByteString as BS
import Data.Version
import Paths_kif_parser

import Text.KIF
import Text.KIF.Parser

main :: IO ()
main = do
  args <- getArgs
  case args of
       ["--version"] -> putStrLn (showVersion version)
       ["-o", "junit", filePath] -> readKIF filePath >>= kifToJUnit "" >>= BS.putStr
       ["-o", "markdown", filePath] -> readKIF filePath >>= kifToMarkdown "" >>= BS.putStr
       ["-o", "markdown", "-test", config, filePath] ->
         readKIF filePath >>= kifToMarkdown config >>= BS.putStr
       ["-o", "json", filePath] -> readKIF filePath >>= kifToJSON "" >>= BS.putStr
       _ -> putStrLn "Usage : kif-parser -o [junit|markdown|json] <kif-log-file>"

kifToJUnit :: String -> KIFTest -> IO BS.ByteString
kifToJUnit config kifTest = do
  timeStamp <- getClockTime
  hostName <- getHostName
  context <- makeContext $ do
    "configuration" =: config
    "timeStamp"     =: show timeStamp
    "hostName"      =: hostName
    "test"          =: mapStrings escapeXML kifTest
  filePath <- getDataFileName "junit.tmpl"
  evalTemplate filePath context

escapeXML :: String -> String
escapeXML = concatMap esc
 where
  esc c = case c of
      '"'  -> "&quot;"
      '\'' -> "&apos;"
      '<'  -> "&lt;"
      '>'  -> "&gt;"
      '&'  -> "&amp;"
      _    -> [c]

kifToMarkdown :: String -> KIFTest -> IO BS.ByteString
kifToMarkdown config kifTest = do
  context <- makeContext $ do
    "configuration" =: config
    "test" =: kifTest
  filePath <- getDataFileName "markdown.tmpl"
  evalTemplate filePath context

kifToJSON :: String -> KIFTest -> IO BS.ByteString
kifToJSON config kifTest = do
  context <- makeContext $ do
    "configuration" =: escapeJSON config
    "test"          =: mapStrings escapeJSON kifTest
  filePath <- getDataFileName "json.tmpl"
  evalTemplate filePath context

escapeJSON :: String -> String
escapeJSON str = "\"" ++ concatMap esc str ++ "\""
 where
  esc c = case c of
      '"'    -> "\\\""
      '\\'   -> "\\\\"
      '/'    -> "\\/"
      '\NUL' -> "\\u0000"
      '\SOH' -> "\\u0001"
      '\STX' -> "\\u0002"
      '\ETX' -> "\\u0003"
      '\EOT' -> "\\u0004"
      '\ENQ' -> "\\u0005"
      '\ACK' -> "\\u0006"
      '\a'   -> "\\u0007"
      '\b'   -> "\\b"
      '\t'   -> "\\t"
      '\n'   -> "\\n"
      '\v'   -> "\\u000b"
      '\f'   -> "\\f"
      '\r'   -> "\\r"
      '\SO'  -> "\\u000e"
      '\SI'  -> "\\u000f"
      '\DLE' -> "\\u0010"
      '\DC1' -> "\\u0011"
      '\DC2' -> "\\u0012"
      '\DC3' -> "\\u0013"
      '\DC4' -> "\\u0014"
      '\NAK' -> "\\u0015"
      '\SYN' -> "\\u0016"
      '\ETB' -> "\\u0017"
      '\CAN' -> "\\u0018"
      '\EM'  -> "\\u0019"
      '\SUB' -> "\\u001a"
      '\ESC' -> "\\u001b"
      '\FS'  -> "\\u001c"
      '\GS'  -> "\\u001d"
      '\RS'  -> "\\u001e"
      '\US'  -> "\\u001f"
      '\DEL' -> "\\u007f"
      _      -> [c]
