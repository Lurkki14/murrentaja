{-# LANGUAGE OverloadedStrings #-}

module Gemination 
  (applySpecialGemination, commonGeminated) where

import Control.Applicative
import Data.Maybe
import Data.Text hiding (take, drop, elem, replicate, length, foldr)
import qualified Data.Text as T

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
  "äy", "eu", "ou", "au", "yi", "ui", "iy", "iu" ] :: [Text] 

consonants = [ "b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x" ] :: [T.Text]

vowels = [ "a", "e", "i", "o", "u", "y", "ä", "ö" ] :: [Text]

commonGeminated :: LangWord -> Maybe Text
commonGeminated word = applyCommonGemination <$> parseCommonGeminable word

applyCommonGemination :: CommonGeminable -> Text
applyCommonGemination (CommonGeminable first second tail) =
  append (toText first) $ append secondGeminated tail where
    secondGeminated = append (T.replicate 2 $ T.take 1 (toText second)) (T.drop 1 $ toText second)

parseCommonGeminable :: LangWord -> Maybe CommonGeminable
parseCommonGeminable word =
  liftA2
  (\ms lo -> CommonGeminable ms lo
      (dropPrefix (append (toText ms) (toText lo)) word)) maybeShort longOpen where
    candidate = T.take 5 word
    maybeShort :: Maybe MaybeShort
    maybeShort = parseMaybeShort candidate
    longOpen :: Maybe LongOpen
    longOpen = maybeShort >>= \ms -> parseLongOpen (dropPrefix (toText ms) word)
    dropPrefix prefix = T.drop (T.length prefix)

parseMaybeShort :: Text -> Maybe MaybeShort
parseMaybeShort x = parseV x <|> parseCV x where
  parseV x
    | elem (T.take 1 x) vowels = Just $ V $ T.take 1 x
    | otherwise = Nothing
  parseCV x
    | (elem (T.take 1 x) consonants) && (elem (T.take 1 $ T.drop 1 x) vowels) = Just $ CV $ T.take 2 x
    | otherwise = Nothing

parseLongOpen :: Text -> Maybe LongOpen
parseLongOpen x
  | not $ elem consonantCandidate consonants = Nothing
  | otherwise = parsedVV <|> parsedDD where
  consonantCandidate = T.take 1 x
  vvddCandidate = T.drop 1 $ T.take 3 x
  parsedVV = fmap (CVV . append consonantCandidate) (parseLongVowel vvddCandidate)
  parsedDD = fmap (CDD . append consonantCandidate) (parseDiphthong vvddCandidate)

-- For "general gemination" we need to know if:
-- 1. The preceding syllable is short (CV or V)
-- 2. Whether the preceding syllable stressed (first)
-- 3. If the second syllable is of the form (CVV or CDD*) *D = part of a diphthong
-- In short, 'CVCDD, 'CVCVV, 'VCDD, 'VCVV are applicable

parseDiphthong :: Text -> Maybe Text
parseDiphthong x
  | T.length candidate /= 2 = Nothing
  | not $ elem candidate diphthongs = Nothing
  | elem candidate diphthongs = Just x
  where candidate = T.take 2 x -- Text.length is O(n) so don't waste time calculating the length of a big string

parseLongVowel :: Text -> Maybe Text
parseLongVowel x
  | bothSame candidate && elem (T.take 1 x) vowels = Just x
  | otherwise = Nothing where
  bothSame x = T.take 1 x == T.drop 1 x
  candidate = T.take 2 x

-- TODO: Very similar code to 'applyEpenthesis', if there's more of these,
-- make a common function for this type of accumulating function
applySpecialGemination :: Text -> Maybe Text
applySpecialGemination word
  | geminated == word = Nothing
  | otherwise = Just geminated where
  geminated = go (T.take 1 word) $ T.drop 1 word -- Initial consonant cannot be geminated
  go :: Text -> Text -> Text
  go acc "" = acc
  go acc text =
    go (append acc $ fst $ nextAcc text) (T.drop (snd $ nextAcc text) text)
  nextAcc text
    | isDoubledChar $ T.take 2 text = (,) (T.take 2 text) 2 -- Don't geminate when already geminated
    | otherwise =
        maybe
          ((,) (T.take 1 text) 1)
          (\lo -> (,) (doSpecialGemination $ toText lo) 3)
          (parseLongOpen $ T.take 3 text)
  doSpecialGemination text = T.replicate 2 (T.take 1 text) `append` T.drop 1 text
  isDoubledChar text = T.take 1 text == T.drop 1 text
