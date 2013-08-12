module Text.KIF.Parser where

import Control.Applicative
import Control.Monad (replicateM)
import Text.Parsec hiding (many, (<|>))
import Text.Parsec.String

import Text.KIF

readKIF :: FilePath -> IO KIFTest
readKIF kifFile = do
  content <- readFile kifFile
  case runParser pTest () kifFile content of
       Left err  -> error ("Parser error: " ++ show err)
       Right res -> return res

pTest :: Parser KIFTest
pTest = do
  pPrefix
  string "BEGIN KIF TEST RUN:"
  space
  noScenarios <- pInt
  space
  string "scenarios"
  spaces
  pPrefix
  scenarios <- replicateM noScenarios pScenario
  pSeparator
  pPrefix
  string "KIF TEST RUN FINISHED:"
  space
  failures <- pInt
  space
  string "failures"
  space
  duration <- between (char '(') (char ')') (string "duration" *> space *> pDuration)
  spaces
  pSeparator
  return (KIFTest noScenarios failures duration scenarios)

pScenario :: Parser KIFScenario
pScenario = do
  pSeparator
  (no, noSteps) <- pBeginScenario
  message <- pLine
  pSeparator
  steps <- many (try (pPrefix >> (pSuccess <|> pFail)))
  pSeparator
  time <- pEndScenario
  pSeparator
  pPrefix
  spaces
  return (KIFScenario message no noSteps time steps)

pBeginScenario :: Parser (Int, Int)
pBeginScenario = do
  pPrefix
  string "BEGIN SCENARIO "
  no <- scenarioNumber
  steps <- between (char '(') (char ')') (pInt <* space <* string "steps")
  spaces
  return (no, steps)

pEndScenario :: Parser Float
pEndScenario = do
  pPrefix
  string "END OF SCENARIO"
  space
  duration <- between (char '(') (char ')') (string "duration" *> space *> pDuration)
  spaces
  return duration

pPrefix :: Parser ()
pPrefix = do
  pDate
  space
  pTime
  space
  manyTill anyChar space
  spaces
  return ()

pDate :: Parser ()
pDate = do
  replicateM 4 digit
  char '-'
  replicateM 2 digit
  char '-'
  replicateM 2 digit
  return ()

pTime :: Parser ()
pTime = do
  replicateM 2 digit
  char ':'
  replicateM 2 digit
  char ':'
  replicateM 2 digit
  char '.'
  replicateM 3 digit
  return ()

pSuccess :: Parser KIFStep
pSuccess = do
  string "PASS"
  space
  time <- between (char '(') (char ')') pDuration
  char ':'
  space
  message <- pMessage
  return (Pass message time)

pFail :: Parser KIFStep
pFail = do
  string "FAIL"
  space
  time <- between (char '(') (char ')') pDuration
  char ':'
  space
  message <- pMessage
  pPrefix
  string "FAILING ERROR:"
  space
  reason <- pMessage
  return (Fail message time reason)

pMessage :: Parser String
pMessage = do 
  mes <- manyTill anyChar newline
  spaces
  return mes

pLine :: Parser String
pLine = pPrefix *> pMessage

pSeparator :: Parser ()
pSeparator = pPrefix >> string (replicate 51 '-') >> spaces >> return ()

scenarioNumber :: Parser Int
scenarioNumber = pInt <* char '/' <* pInt <* spaces

pDuration :: Parser Float
pDuration = pFloat <* char 's'

pInt :: Parser Int
pInt = read <$> many1 digit

pFloat :: Parser Float
pFloat = do
  integer <- many1 digit
  char '.'
  fractional <- many1 digit
  return (read (integer ++ "." ++ fractional))
