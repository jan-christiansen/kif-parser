module Text.KIF where

import Text.XML.Light
import System.Time
import Network.HostName (HostName)

data KIFTest = KIFTest Int       -- no of scenarios
                       Int       -- failures
                       Float     -- duration
                       [KIFScenario]
  deriving (Eq, Show)

data KIFScenario = KIFScenario String -- explanation
                               Int    -- no
                               Int    -- no of steps
                               Float  -- duration
                               [KIFStep]
  deriving (Eq, Show)

data KIFStep = Pass String Float
             | Fail String Float String
  deriving (Eq, Show)

class Markdown a where
  toMarkdown :: a -> String

instance Markdown KIFTest where
  toMarkdown (KIFTest _ _ _ scenarios) =
   "# Test Specification\n"
     ++ unlines (map toMarkdown scenarios)

instance Markdown KIFScenario where
  toMarkdown (KIFScenario name no noOfSteps duration steps) = 
    "## Test Case " ++ show no ++ ": " ++ name ++ "\n"
      ++ "### Performs " ++ show noOfSteps ++ " steps in " ++ show duration ++ "s\n"
      ++ unlines (map toMarkdown steps)

instance Markdown KIFStep where
  toMarkdown (Pass message _) = " * **Passed Step:** " ++ message
  toMarkdown (Fail message _ _) = " * **Failing Step:** " ++ message


toJUnit :: HostName -> ClockTime -> KIFTest -> String
toJUnit hostName time (KIFTest noScenarios failures duration scenarios) =
  showTopElement (add_attrs [Attr (unqual "name") "KIFTest",
                             Attr (unqual "errors") "0",
                             Attr (unqual "failures") (show failures),
                             Attr (unqual "tests") (show noScenarios),
                             Attr (unqual "time") (show duration),
                             Attr (unqual "timestamp") (show time),
                             Attr (unqual "hostname") hostName]
                            (node (unqual "testsuite") (map testcase scenarios)))
 where
  testcase (KIFScenario explanation _ _ sDuration _) = 
    add_attrs [Attr (unqual "name") explanation,
               Attr (unqual "classname") "KIFTestScenario",
               Attr (unqual "time") (show sDuration)] 
              (node (unqual "testcase") ())
