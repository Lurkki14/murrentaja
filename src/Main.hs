{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Char as T
import Data.Maybe
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Options.Applicative

import Features

-- Program options
data Input =
  File FilePath |
  StdIn |
  Interactive deriving (Show)

-- Language features
-- Associate all these with LangWord -> Maybe Text (Maybe because replacing is expensive)
-- We also might need to model dependencies and conflicts
data PhonologicalFeatures = PhonologicalFeatures {
  commonGemination :: Bool,
  specialGemination :: Bool,
  epenthesis :: Bool -- https://kaino.kotus.fi/visk/sisallys.php?p=33
}

-- Being able to parse CV | V at the beginning of a word does not mean short syllable, hence the name
data MaybeShort = CV T.Text | V T.Text deriving (Show) 
-- this kind of syllable is applicable for common gemination
data LongOpen = CVV T.Text | CDD T.Text deriving (Show) 
data Syllable = MaybeShort | LongOpen | LongOther
data CommonGeminable = CommonGeminable {
  first :: MaybeShort,
  second :: LongOpen,
  tail :: T.Text
} deriving (Show)

data EpenthesisReady = EpenthesisReady {
  init :: T.Text, -- can be empty (probably never will though)
  epenthesisVowel :: T.Char,
  epenthesisTrigger :: T.Text,
  tail :: T.Char
}

type LangWord = T.Text -- smart constructor could be useful

class ToText a where
  toText :: a -> T.Text

instance ToText MaybeShort where
  toText (CV x) = x
  toText (V x) = x

instance ToText LongOpen where
  toText (CVV x) = x
  toText (CDD x) = x

-- Other vowel combinations aren't considered diphthongs and will break a syllable (eg. ta-e)
-- TODO: replace these with data structures that are more efficient to search
diphthongs = [ "ei", "öi", "äi", "oi", "ai", "ey", "öy",
  "äy", "eu", "ou", "au", "yi", "ui", "iy", "iu" ] :: [T.Text] 

consonants = [ "b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x" ] :: [T.Text]

vowels = [ "a", "e", "i", "o", "u", "y", "ä", "ö" ] :: [T.Text]
vowelsChar = [ 'a', 'e', 'i', 'o', 'u', 'y', 'ä', 'ö' ] :: [T.Char]

epenthesisTriggers = [ "lp", "lk", "lm", "lv", "nh" ] :: [T.Text]

input :: Parser Input
input = file <|> stdIn <|> interactive where
  file = File <$> strOption (
    long "file" <>
    short 'f' )
  stdIn = flag' StdIn (
    long "stdin" <>
    short 's' )
  interactive = flag' Interactive (
    long "interactive" <>
    short 'i' )

options :: ParserInfo Input
options = info (input <**> helper) mempty

-- TODO: breaks when there are multiple epentheses to be applied
applyEpenthesis :: LangWord -> Maybe T.Text
applyEpenthesis word = Just $ go "" word where
  -- TODO: Make types for vowels and epenthesis triggers since this is quite hard to understand
  go :: T.Text -> T.Text -> T.Text
  go acc text = 
    case T.uncons text of
      (Just (char, next)) -> if elem char vowelsChar && elem (T.take 2 next) epenthesisTriggers
                             then go (T.append acc $ doEpenthesis char next) $ T.drop 3 next
                             else go (T.append acc $ T.singleton char) next -- TODO: seek to next vowel
      Nothing -> acc
  doEpenthesis char fromTrigger =
    T.singleton char `T.append` T.take 1 fromTrigger `T.append` T.singleton char `T.append` T.drop 1 fromTrigger

applyCommonGemination :: CommonGeminable -> T.Text
applyCommonGemination (CommonGeminable first second tail) =
  T.append (toText first) $ T.append secondGeminated tail where
    secondGeminated = T.append (T.replicate 2 $ T.take 1 (toText second)) (T.drop 1 $ toText second)

parseCommonGeminable :: LangWord -> Maybe CommonGeminable
parseCommonGeminable word =
  liftA2
  (\ms lo -> CommonGeminable ms lo (dropPrefix (T.append (toText ms) (toText lo)) word)) maybeShort longOpen where
    candidate = T.take 5 word
    maybeShort :: Maybe MaybeShort
    maybeShort = parseMaybeShort candidate
    longOpen :: Maybe LongOpen
    longOpen = maybeShort >>= \ms -> parseLongOpen (dropPrefix (toText ms) word)
    dropPrefix prefix = T.drop (T.length prefix)

parseMaybeShort :: T.Text -> Maybe MaybeShort
parseMaybeShort x = parseV x <|> parseCV x where
  parseV x
    | elem (T.take 1 x) vowels = Just $ V $ T.take 1 x
    | otherwise = Nothing
  parseCV x
    | (elem (T.take 1 x) consonants) && (elem (T.take 1 $ T.drop 1 x) vowels) = Just $ CV $ T.take 2 x
    | otherwise = Nothing

parseLongOpen :: T.Text -> Maybe LongOpen
parseLongOpen x
  | not $ elem consonantCandidate consonants = Nothing
  | otherwise = parsedVV <|> parsedDD where
  consonantCandidate = T.take 1 x
  vvddCandidate = T.drop 1 $ T.take 3 x
  parsedVV = fmap (CVV . T.append consonantCandidate) (parseLongVowel vvddCandidate)
  parsedDD = fmap (CDD . T.append consonantCandidate) (parseDiphthong vvddCandidate)

-- For "general gemination" we need to know if:
-- 1. The preceding syllable is short (CV or V)
-- 2. Whether the preceding syllable stressed (first)
-- 3. If the second syllable is of the form (CVV or CDD*) *D = part of a diphthong
-- In short, 'CVCDD, 'CVCVV, 'VCDD, 'VCVV are applicable

parseDiphthong :: T.Text -> Maybe T.Text
parseDiphthong x
  | T.length candidate /= 2 = Nothing
  | not $ elem candidate diphthongs = Nothing
  | elem candidate diphthongs = Just x
  where candidate = T.take 2 x -- Text.length is O(n) so don't waste time calculating the length of a big string

parseLongVowel :: T.Text -> Maybe T.Text
parseLongVowel x
  | bothSame candidate && elem (T.take 1 x) vowels = Just x
  | otherwise = Nothing where
  bothSame x = T.take 1 x == T.drop 1 x
  candidate = T.take 2 x

commonGeminateText :: T.Text -> T.Text
commonGeminateText text =
  foldr (\(orig, gem) acc -> T.replace orig gem acc) text $ replacements text

replacements :: T.Text -> [(T.Text, T.Text)]
replacements text = [ (orig, gem) | (orig, Just gem) <- zip words geminated ] where
  words = T.words text
  geminated = fmap applyCommonGemination . parseCommonGeminable <$> words

interactiveLoop :: IO ()
interactiveLoop =
  T.IO.getLine >>= T.IO.putStrLn . ("-> " <>) . commonGeminateText >> interactiveLoop

main = do
  options <- execParser options
  doMain options where
    doMain Interactive = T.IO.putStrLn "Enter a line of text: " >> interactiveLoop
    doMain StdIn = pure () -- TODO: do something :D
    doMain (File filePath) = T.IO.readFile filePath >>= T.IO.putStr . commonGeminateText
