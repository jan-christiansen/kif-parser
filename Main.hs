{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Environment (getArgs, withArgs)
import System.Time
import Data.Version (showVersion)
import Network.HostName
import Text.Twine
import qualified Data.ByteString as BS
import System.Console.CmdArgs
import Paths_kif_parser

import Text.KIF
import Text.KIF.Parser

data Params = Params {
  output :: OutputType,
  configuration :: String,
  file :: FilePath
}
  deriving (Show, Data, Typeable)

data OutputType = JUnit
                | Markdown
                | JSON
  deriving (Show, Data, Typeable)

params :: Params
params = Params {
  configuration = "" &= help "An optional text that is displayed as the test description",
  output = JUnit &= argPos 0,
  file = def &= typ "FILE" &= argPos 1
}

getParams :: IO Params
getParams =
  cmdArgs $ params
          &= program "kif-parser"
          &= versionArg [explicit, name "version", name "v", summary summaryText]
          &= summary summaryText
          &= details ["Process Keep It Functional log files"]

summaryText :: String
summaryText = "kif-parser " ++ showVersion version ++ ", Jan Christiansen"

main :: IO ()
main = do
  args <- getArgs
  params <- (if null args then withArgs ["--help"] else id) getParams
  case output params of
       JUnit    -> readKIF (file params) >>= kifToJUnit (configuration params) >>= BS.putStr
       Markdown -> readKIF (file params) >>= kifToMarkdown (configuration params) >>= BS.putStr
       JSON     -> readKIF (file params) >>= kifToJSON (configuration params) >>= BS.putStr

kifToJUnit :: String -> KIFTest -> IO BS.ByteString
kifToJUnit config kifTest = do
  timeStamp <- getClockTime
  hostName <- getHostName
  context <- makeContext $ do
    "configuration" =: config
    "timeStamp"     =: show timeStamp
    "hostName"      =: hostName
    "test"          =: mapStrings escapeXML kifTest
  filePath <- getDataFileName "templates/junit.tmpl"
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
  filePath <- getDataFileName "templates/markdown.tmpl"
  evalTemplate filePath context

kifToJSON :: String -> KIFTest -> IO BS.ByteString
kifToJSON config kifTest = do
  context <- makeContext $ do
    "configuration" =: escapeJSON config
    "test"          =: mapStrings escapeJSON kifTest
  filePath <- getDataFileName "templates/json.tmpl"
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
