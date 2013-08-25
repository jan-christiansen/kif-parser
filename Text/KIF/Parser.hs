module Text.KIF.Parser where

import Control.Applicative
import Control.Monad (replicateM)
import Text.Parsec hiding (many, (<|>))
import Text.Parsec.String

import Text.KIF

readKIF :: FilePath -> IO KIFTest
readKIF kifFile = do
  content <- readFile kifFile
  case parse pTest kifFile content of
       Left err  -> error ("Parser error: " ++ show err)
       Right res -> return res

pTest :: Parser KIFTest
pTest = do
  string "BEGIN KIF TEST RUN:"
  space
  noOfScenarios <- pInt
  space
  string "scenarios"
  spaces
  scenarios <- replicateM noOfScenarios pScenario
  pSeparator
  string "KIF TEST RUN FINISHED:"
  space
  noOfFailures <- pInt
  space
  string "failures"
  space
  duration <- between (char '(') (char ')') (string "duration" *> space *> pDuration)
  spaces
  pSeparator
  return (KIFTest noOfScenarios noOfFailures duration scenarios)

pScenario :: Parser KIFScenario
pScenario = do
  pSeparator
  (no, noOfSteps) <- pBeginScenario
  description <- pLine
  pSeparator
  steps <- many (pSuccess <|> pFail)
  pSeparator
  duration <- pEndScenario
  pSeparator
  spaces
  return (KIFScenario no description noOfSteps duration (all stepPassed steps) steps)

pBeginScenario :: Parser (Int, Int)
pBeginScenario = do
  string "BEGIN SCENARIO "
  no <- scenarioNumber
  steps <- between (char '(') (char ')') (pInt <* space <* string "steps")
  spaces
  return (no, steps)

pEndScenario :: Parser Float
pEndScenario = do
  string "END OF SCENARIO"
  space
  duration <- between (char '(') (char ')') (string "duration" *> space *> pDuration)
  spaces
  return duration

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
  duration <- between (char '(') (char ')') pDuration
  char ':'
  space
  message <- pMessage
  string "FAILING ERROR:"
  space
  reason <- pMessage
  return (Fail message reason duration)

pMessage :: Parser String
pMessage = do
  mes <- manyTill anyChar newline
  spaces
  return mes

pLine :: Parser String
pLine = pMessage

pSeparator :: Parser ()
pSeparator = string (replicate 51 '-') >> spaces >> return ()

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
