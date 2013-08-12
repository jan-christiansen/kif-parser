module Tests where

import Test.HUnit
import Text.Parsec
import Text.Parsec.String

import Text.KIF
import Text.KIF.Parser

main :: IO ()
main = do
  -- runTestTT tests
  print (testParser pTest testTest)
  putStr (toMarkdown (unRight (testParser pTest testTest)))
  print (toJUnit (unRight (testParser pTest testTest)))
  -- print (testParser pScenario testScenario1)
  -- print (testParser pScenario testScenario2)

unRight (Right x) = x

instance Eq ParseError where
  _ == _ = False

testParser :: Parser a -> String -> Either ParseError a
testParser parser = runParser parser () "Test"

tests :: Test
tests =  test [
  -- "KIFTest" ~: Right (KIFTest []) ~=? testParser pBeginTest "2013-08-09 18:05:35.442 HC-App-Integration-Tests[33056:c07] BEGIN KIF TEST RUN: 2 scenarios\n",
  "KIFScenarioBegin" ~: Right (1, 6) ~=? testParser pBeginScenario "2013-08-09 18:05:35.442 HC-App-Integration-Tests[33056:c07] BEGIN SCENARIO 1/2 (6 steps)\n",
  "KIFScenarioEnd" ~: Right 2.53 ~=? testParser pEndScenario "2013-08-09 18:05:35.442 HC-App-Integration-Tests[33056:c07] END OF SCENARIO (duration 2.53s)\n",
  "KIFStep" ~: Right (Pass "Message" 2.63) ~=? testParser pSuccess "2013-08-09 18:05:35.442 HC-App-Integration-Tests[33056:c07] PASS (2.63s): Message\n"
 ]

testFail :: String
testFail =
  "2013-08-10 09:55:36.438 HC-App-Integration-Tests[40250:c07] FAIL (10.01s): Wait for view with accessibility label \"Fail\"\n\
  \2013-08-10 09:55:36.438 HC-App-Integration-Tests[40250:c07] FAILING ERROR: Error Domain=KIFTest Code=0 \"The step timed out after 10.00 seconds.\" UserInfo=0x99c8c60 {NSUnderlyingError=0x99909e0 \"Waiting for presence of accessibility element with label \"\"\", NSLocalizedDescription=The step timed out after 10.00 seconds.}"

testTest :: String
testTest =
  "2013-08-09 18:05:35.442 HC-App-Integration-Tests[33056:c07] BEGIN KIF TEST RUN: 2 scenarios\n\
  \2013-08-09 18:05:35.443 HC-App-Integration-Tests[33056:c07] \n" ++ testScenario1 ++
  "2013-08-09 18:05:40.674 HC-App-Integration-Tests[33056:c07] \n" ++ testScenario2 ++
  "2013-08-09 18:05:45.298 HC-App-Integration-Tests[33056:c07] \n\
  \2013-08-09 18:05:45.298 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------\n\
  \2013-08-09 18:05:45.299 HC-App-Integration-Tests[33056:c07] KIF TEST RUN FINISHED: 0 failures (duration 9.88s)\n\
  \2013-08-09 18:05:45.299 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------\n\
  \2013-08-09 18:05:45.299 HC-App-Integration-Tests[33056:c07] *** KIF TESTING FINISHED: 0 failures\n"

testScenario1 :: String
testScenario1 =
  "2013-08-09 18:05:40.675 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------\n\
  \2013-08-09 18:05:35.442 HC-App-Integration-Tests[33056:c07] BEGIN SCENARIO 1/2 (6 steps)\n\
  \2013-08-09 18:05:35.442 HC-App-Integration-Tests[33056:c07] Check whether the user can log in.\n\
  \2013-08-09 18:05:35.442 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------\n\
  \2013-08-09 18:05:35.442 HC-App-Integration-Tests[33056:c07] PASS (2.63s): Type the text \"max.mustermann@ma-design.de\" into the view with accessibility label \"E-Mail / Username\"\n\
  \2013-08-09 18:05:35.442 HC-App-Integration-Tests[33056:c07] PASS (1.00s): Type the text \"password\" into the view with accessibility label \"Password\"\n\
  \2013-08-09 18:05:35.442 HC-App-Integration-Tests[33056:c07] PASS (0.53s): Tap view with accessibility label \"Login\"\n\
  \2013-08-10 09:55:36.438 HC-App-Integration-Tests[40250:c07] FAIL (10.01s): Wait for view with accessibility label \"Fail\"\n\
  \2013-08-10 09:55:36.438 HC-App-Integration-Tests[40250:c07] FAILING ERROR: Error Domain=KIFTest Code=0 \"The step timed out after 10.00 seconds.\" UserInfo=0x99c8c60 {NSUnderlyingError=0x99909e0 \"Waiting for presence of accessibility element with label \"\"\", NSLocalizedDescription=The step timed out after 10.00 seconds.}\n\
  \2013-08-09 18:05:35.442 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------\n\
  \2013-08-09 18:05:35.442 HC-App-Integration-Tests[33056:c07] END OF SCENARIO (duration 5.23s)\n\
  \2013-08-09 18:05:35.442 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------\n"

testScenario2 :: String
testScenario2 =
  "2013-08-09 18:05:40.675 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------\n\
  \2013-08-09 18:05:40.675 HC-App-Integration-Tests[33056:c07] BEGIN SCENARIO 2/2 (8 steps)\n\
  \2013-08-09 18:05:40.676 HC-App-Integration-Tests[33056:c07] Check whether the input is deleted after logout.\n\
  \2013-08-09 18:05:40.676 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------\n\
  \2013-08-09 18:05:42.711 HC-App-Integration-Tests[33056:c07] PASS (2.03s): Type the text \"max.mustermann@ma-design.de\" into the view with accessibility label \"E-Mail / Username\"\n\
  \2013-08-09 18:05:43.684 HC-App-Integration-Tests[33056:c07] PASS (0.97s): Type the text \"password\" into the view with accessibility label \"Password\"\n\
  \2013-08-09 18:05:44.200 HC-App-Integration-Tests[33056:c07] PASS (0.52s): Tap view with accessibility label \"Login\"\n\
  \2013-08-09 18:05:44.726 HC-App-Integration-Tests[33056:c07] PASS (0.53s): Tap view with accessibility label \"Additional developer options\"\n\
  \2013-08-09 18:05:45.261 HC-App-Integration-Tests[33056:c07] PASS (0.53s): Tap view with accessibility label \"Logout\"\n\
  \2013-08-09 18:05:45.273 HC-App-Integration-Tests[33056:c07] PASS (0.01s): Wait for view with accessibility label \"kif_start_login_view\"\n\
  \2013-08-09 18:05:45.285 HC-App-Integration-Tests[33056:c07] PASS (0.01s): Check that text field with accessibility label \"E-Mail / Username\" contains text \"\"\n\
  \2013-08-09 18:05:45.296 HC-App-Integration-Tests[33056:c07] PASS (0.01s): Check that text field with accessibility label \"Password\" contains text \"\"\n\
  \2013-08-09 18:05:45.297 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------\n\
  \2013-08-09 18:05:45.297 HC-App-Integration-Tests[33056:c07] END OF SCENARIO (duration 4.62s)\n\
  \2013-08-09 18:05:45.298 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------\n"


-- 2013-08-09 18:05:35.442 HC-App-Integration-Tests[33056:c07] BEGIN KIF TEST RUN: 2 scenarios
-- 2013-08-09 18:05:35.443 HC-App-Integration-Tests[33056:c07]
-- 2013-08-09 18:05:35.443 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------
-- 2013-08-09 18:05:35.443 HC-App-Integration-Tests[33056:c07] BEGIN SCENARIO 1/2 (6 steps)
-- 2013-08-09 18:05:35.443 HC-App-Integration-Tests[33056:c07] Check whether the user can log in.
-- 2013-08-09 18:05:35.444 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------
-- 2013-08-09 18:05:38.071 HC-App-Integration-Tests[33056:c07] PASS (2.63s): Type the text "max.mustermann@ma-design.de" into the view with accessibility label "E-Mail / Username"
-- 2013-08-09 18:05:39.067 HC-App-Integration-Tests[33056:c07] PASS (1.00s): Type the text "password" into the view with accessibility label "Password"
-- 2013-08-09 18:05:39.594 HC-App-Integration-Tests[33056:c07] PASS (0.53s): Tap view with accessibility label "Login"
-- 2013-08-09 18:05:39.606 HC-App-Integration-Tests[33056:c07] PASS (0.01s): Wait for view with accessibility label "kif_appliances_view"
-- 2013-08-09 18:05:40.146 HC-App-Integration-Tests[33056:c07] PASS (0.54s): Tap view with accessibility label "Additional developer options"
-- 2013-08-09 18:05:40.671 HC-App-Integration-Tests[33056:c07] PASS (0.53s): Tap view with accessibility label "Logout"
-- 2013-08-09 18:05:40.672 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------
-- 2013-08-09 18:05:40.673 HC-App-Integration-Tests[33056:c07] END OF SCENARIO (duration 5.23s)
-- 2013-08-09 18:05:40.674 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------
-- 2013-08-09 18:05:40.674 HC-App-Integration-Tests[33056:c07]
-- 2013-08-09 18:05:40.675 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------
-- 2013-08-09 18:05:40.675 HC-App-Integration-Tests[33056:c07] BEGIN SCENARIO 2/2 (8 steps)
-- 2013-08-09 18:05:40.676 HC-App-Integration-Tests[33056:c07] Check whether the input is deleted after logout.
-- 2013-08-09 18:05:40.676 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------
-- 2013-08-09 18:05:42.711 HC-App-Integration-Tests[33056:c07] PASS (2.03s): Type the text "max.mustermann@ma-design.de" into the view with accessibility label "E-Mail / Username"
-- 2013-08-09 18:05:43.684 HC-App-Integration-Tests[33056:c07] PASS (0.97s): Type the text "password" into the view with accessibility label "Password"
-- 2013-08-09 18:05:44.200 HC-App-Integration-Tests[33056:c07] PASS (0.52s): Tap view with accessibility label "Login"
-- 2013-08-09 18:05:44.726 HC-App-Integration-Tests[33056:c07] PASS (0.53s): Tap view with accessibility label "Additional developer options"
-- 2013-08-09 18:05:45.261 HC-App-Integration-Tests[33056:c07] PASS (0.53s): Tap view with accessibility label "Logout"
-- 2013-08-09 18:05:45.273 HC-App-Integration-Tests[33056:c07] PASS (0.01s): Wait for view with accessibility label "kif_start_login_view"
-- 2013-08-09 18:05:45.285 HC-App-Integration-Tests[33056:c07] PASS (0.01s): Check that text field with accessibility label "E-Mail / Username" contains text ""
-- 2013-08-09 18:05:45.296 HC-App-Integration-Tests[33056:c07] PASS (0.01s): Check that text field with accessibility label "Password" contains text ""
-- 2013-08-09 18:05:45.297 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------
-- 2013-08-09 18:05:45.297 HC-App-Integration-Tests[33056:c07] END OF SCENARIO (duration 4.62s)
-- 2013-08-09 18:05:45.298 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------
-- 2013-08-09 18:05:45.298 HC-App-Integration-Tests[33056:c07]
-- 2013-08-09 18:05:45.298 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------
-- 2013-08-09 18:05:45.299 HC-App-Integration-Tests[33056:c07] KIF TEST RUN FINISHED: 0 failures (duration 9.88s)
-- 2013-08-09 18:05:45.299 HC-App-Integration-Tests[33056:c07] ---------------------------------------------------
-- 2013-08-09 18:05:45.299 HC-App-Integration-Tests[33056:c07] *** KIF TESTING FINISHED: 0 failures