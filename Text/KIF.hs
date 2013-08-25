{-# LANGUAGE MultiParamTypeClasses,
             FlexibleInstances,
             OverloadedStrings #-}

module Text.KIF where

import Numeric
import Text.Twine

data KIFTest = KIFTest {
  noOfScenarios :: Int,
  noOfFailures :: Int,
  testDuration :: Float,
  scenarios :: [KIFScenario]
}
  deriving (Eq, Show)

data KIFScenario = KIFScenario {
  number :: Int,
  scenarioDescription :: String,
  noOfSteps :: Int,
  scenarioDuration :: Float,
  scenarioPassed :: Bool,
  steps :: [KIFStep]
}
  deriving (Eq, Show)

data KIFStep = Pass {
  stepDescription :: String,
  stepDuration :: Float
}
             | Fail {
  stepDescription :: String,
  reason :: String,
  stepDuration :: Float
}
  deriving (Eq, Show)

stepPassed :: KIFStep -> Bool
stepPassed (Pass _ _)    = True
stepPassed (Fail _ _ _ ) = False

mapStrings :: (String -> String) -> KIFTest -> KIFTest
mapStrings escape (KIFTest noOfScenarios noOfFailures testDuration scenarios) =
  KIFTest noOfScenarios noOfFailures testDuration (map (mapScenarioStrings escape) scenarios)

mapScenarioStrings :: (String -> String) -> KIFScenario -> KIFScenario
mapScenarioStrings escape (KIFScenario number description noOfSteps duration passed steps) =
  KIFScenario number
              (escape description)
              noOfSteps
              duration
              passed
              (map (mapStepStrings escape) steps)

mapStepStrings :: (String -> String) -> KIFStep -> KIFStep
mapStepStrings escape (Pass description duration) =
  Pass (escape description) duration
mapStepStrings escape (Fail description reason duration) =
  Fail (escape description) (escape reason) duration

instance TemplateInterface IO KIFTest where
  property "noOfScenarios" = return . bind . noOfScenarios
  property "noOfFailures"  = return . bind . noOfFailures
  property "duration"  = return . bind . testDuration
  property "scenarios"  = return . bind . scenarios

instance TemplateInterface IO KIFScenario where
  property "number" = return . bind . number
  property "description" = return . bind . scenarioDescription
  property "noOfSteps" = return . bind . noOfSteps
  property "duration" = return . bind . scenarioDuration
  property "passed" = return . bind . scenarioPassed
  property "failed" = return . bind . not . scenarioPassed
  property "steps" = return . bind . steps

instance TemplateInterface IO KIFStep where
  property "description" = return . bind . stepDescription
  property "duration" = return . bind . stepDuration
  property "reason" =
    \step -> if stepPassed step then error "Failed step has no reason"
                                else return (bind (reason step))
  property "passed" = return . bind . stepPassed
  property "failed" = return . bind . not . stepPassed

instance TemplateInterface IO Float where
  makeString x = return (showFFloat Nothing x "")
