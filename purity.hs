#!/usr/bin/env runghc

import System.Environment
import Numeric
import Data.Maybe
import Data.Tuple
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as UTF
import qualified Network.HTTP as HTTP
import Control.Monad
import Data.Monoid (mempty)

import Text.Regex.PCRE
import Text.PrettyPrint.HughesPJ as Doc


--------------------------------------------------------------------------------
-- Raw Parsing

sectionRegex ::  UTF.ByteString
sectionRegex = B.pack "<h2><li>([-A-Za-z0-9 \t]*)<(?:/h2|/li|br)>"

questionRegex ::  UTF.ByteString
questionRegex = B.pack $ "<LI> <INPUT TYPE=checkbox NAME=q VALUE=[0-9a-f]+>" ++
                "([\\S\\s]*)</li>"

-- Try to match regex and str. Returns the matched string or nothing
readOne :: B.ByteString -> B.ByteString -> Maybe B.ByteString
readOne regex str = result
  where
    (_, _, _, results) = str =~ regex :: (B.ByteString, B.ByteString, B.ByteString, [B.ByteString])
    result = case results of
      [] -> Nothing
      [s] -> Just s
      _ -> error "Too many matches"

-- Try to read a section
readSection ::  UTF.ByteString -> Maybe UTF.ByteString
readSection = readOne sectionRegex

-- Try to read a question
readQuestion ::  UTF.ByteString -> Maybe UTF.ByteString
readQuestion = readOne questionRegex

-- Data for representing a test
data QuestionStatus = Done | Technicality | WouldDo | Incomplete
                    deriving (Eq, Show, Ord)

data Question = Question {
  questionText :: B.ByteString,
  comment :: B.ByteString,
  status :: QuestionStatus
  } deriving (Eq, Show)

defaultQuestion ::  Question
defaultQuestion = Question mempty mempty Incomplete

data Section = Section {
  title :: B.ByteString,
  caption :: B.ByteString, -- unused
  questions :: [Question]
  } deriving (Eq, Show)

defaultSection ::  Section
defaultSection = Section mempty mempty []

type Test = [Section]

-- Clean up the questions a bit

data CleanState = PostSpace | Normal deriving (Eq, Show)

clean :: B.ByteString -> B.ByteString
clean str = B.pack . strip $ foldr go ("", Normal) (B.unpack str)
  where
    -- Take away leading and trailing whitespace
    strip (str, _) = reverse $ dropWhile (' ' ==)  $
                     reverse $ dropWhile (' ' ==)  str
    -- Merge adjacent whitespace
    go :: Char -> (String, CleanState) -> (String, CleanState)
    go '\t' (s, Normal) = (' ':s, PostSpace)
    go ' ' (s, Normal) = (' ':s, PostSpace)
    go ' ' state@(_, PostSpace) = state
    go '\t' state@(_, PostSpace) = state
    go c (s, PostSpace) = (c:s, Normal)
    go c (s, Normal) = (c:s, Normal)

-- Turn list of lines in html file into a Test
parseTest :: [B.ByteString] -> Test
parseTest l = reverse $ map reverseQuestions $ impl l []
  where
    -- Reverse the order of the questions in a section
    reverseQuestions section =
      section { questions = (reverse . questions) section }
    -- Build the test from a list of lines and a list of existing sections
    impl :: [B.ByteString] -> [Section] -> Test
    -- Base case
    impl [] sections = sections
    impl (line:restLines) sections = let
      -- convenience variables. Can't pattern match! Want to allow []
      -- for sections.
      section = head sections
      restSection = tail sections
      -- Try matching a section
      sectionTitle = readSection line
      newSection = defaultSection { title = fromJust sectionTitle }
      newSectionList = newSection : sections
      -- Try matching a question
      questionText = readQuestion line
      question = defaultQuestion { questionText = clean $
                                                  fromJust questionText }
      newQuestion = section { questions = question:questions section }
      newQuestionList = newQuestion : restSection
      in
       -- Figure out what sort of line we just parsed
       case (sectionTitle, questionText) of
         -- It was a new section
         (Just _, Nothing) -> impl restLines newSectionList
         -- It was a new question
         (Nothing, Just _) -> impl restLines newQuestionList
         -- No clue! Must not matter.
         (Nothing, Nothing) -> impl restLines sections

-- Read the test from the file.
loadTest :: IO Test
loadTest = do
  let url = "http://www.armory.com/tests/500.html"
  raw <- HTTP.simpleHTTP (HTTP.getRequest url) >>= HTTP.getResponseBody
  let rawLines = UTF.lines $ B.pack raw
      sections = parseTest rawLines
  return sections

--------------------------------------------------------------------------------
-- Pretty Printing

-- Mapping of statuses to strings
statusToStringAssoc ::  [(QuestionStatus, String)]
statusToStringAssoc = [
  (Done, "D"),
  (Technicality, "T"),
  (WouldDo, "W"),
  (Incomplete, " ")
  ]
-- And back again!
stringToStatusAssoc ::  [(String, QuestionStatus)]
stringToStatusAssoc = map swap statusToStringAssoc

-- Convert a status into a string for test display
statusToChar :: QuestionStatus -> String
statusToChar = fromJust . flip lookup statusToStringAssoc

-- How many columns should be used to separate sections/questions
columns ::  Int
columns = 79

-- Convert ByteString to Doc
bText :: B.ByteString -> Doc
bText = text . B.unpack

-- Status to Doc
printStatus :: QuestionStatus -> Doc
printStatus = brackets . text . statusToChar

-- What to put between questions and comments
questionCommentSep ::  Doc
questionCommentSep = text $ replicate columns '-'

-- Convert Question to Doc
printQuestion :: Question -> Doc
printQuestion q = questionSep
                  $$ printStatus (status q) <+> bText (questionText q)
                  $$ questionCommentSep
                  $$ bText (comment q)

-- What goes between questions
questionSep ::  Doc
questionSep = text $ replicate columns '='

-- Print a list of questions
printQuestions :: [Question] -> Doc
printQuestions questions = foldl ($$) Doc.empty $ map printQuestion questions

-- What goes between sections
sectionSep :: Doc
sectionSep = text $ replicate columns '#'

-- Convert Section to Doc
printSection :: Section -> Doc
printSection section = sectionSep
                       $$ text "##" <+> bText (title section)
                       $$ sectionSep
                       $$ printQuestions (questions section)

-- Print out a whole test!
printTest :: Test -> Doc
printTest test = fsep $ map printSection test

--------------------------------------------------------------------------------
-- Scoring

-- Convert a string to a status
charToStatus :: String -> QuestionStatus
charToStatus c = fromMaybe (error $ "can't find " ++ c) $ lookup c stringToStatusAssoc

-- Score for each status type
type TestScore = Map.Map QuestionStatus Int

-- Take a string representing the test and compute scores
scoreTest :: B.ByteString -> TestScore
scoreTest raw = let
  -- regex for response
  regex = B.pack "(?:\n|$)\\[([^\n])\\]"
  all :: [[B.ByteString]]
  -- All looks like [["[X]", "X"], ...]
  all = raw =~ regex
  -- matches is just a list of the chars in brackets
  matches = map (!! 1) all
  -- statuses is a list of statuses
  statuses = map (charToStatus . B.unpack) matches
  -- A list for building the map
  accumList = map (\s -> (s, 1)) statuses
  in
   Map.fromListWith (+) accumList

-- Convert a score into a Doc
printScore :: TestScore -> Doc
printScore test = done <+> slash <+> technicality <+> slash
                  <+> wouldDo <+> slash <> slash
                  <+> integer (fromIntegral total)
  where
    -- Safe lookup
    (!) = flip (Map.findWithDefault 0)
    -- Float to string
    niceFloat f = showFFloat (Just 1) f ""

    slash = text "/"

    -- Counts
    total :: Int
    total = fromIntegral $ sum $ Map.elems test
    doneCount = fromIntegral $ test ! Done
    technicalityCount = fromIntegral $ test ! Technicality
    wouldDoCount = fromIntegral $ test ! WouldDo

    countToPercent c = 100 - 100 / fromIntegral total * c
    countToDoc = text . niceFloat . countToPercent

    done = countToDoc doneCount
    technicality = countToDoc $ doneCount + technicalityCount
    wouldDo = countToDoc $ doneCount + technicalityCount + wouldDoCount

--------------------------------------------------------------------------------
-- Main

main ::  IO ()
main = do
  args <- getArgs
  sequence_ $ do
    filePath <- args
    return $ B.readFile filePath >>= print . printScore . scoreTest
  when (null args) $ loadTest >>= print . printTest
