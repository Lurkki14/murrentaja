{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Diphthongs
  (applySavoDiphthongNarrowing, applyWesternDiphthongWidening) where

import Data.Char
import Data.Maybe
import Data.Text hiding (any, elem, drop)
import qualified Data.Text as T
import Prelude hiding (lookup)

import Utils

data VowelHarmony =
  BackHarmony |
  FrontHarmony deriving (Show)

-- Cause eg. poika -> poeka
-- Narrowing, eg. the second element of the diphthong is closer to the first
-- element than in standard Finnish
savoNarrowableDiphthongs = [ "oi", "öi", "ai", "äi" ] :: [Text]
-- työmies -> tyämiäs
-- Opposite of narrowing, meaning tongue movement is greater
westernWideningDiphthongs = [ "ie", "uo", "yö" ] :: [Text]

backVowels = [ 'a', 'o', 'u' ] :: [Char]

-- Check if word contains any back vowel
vowelHarmony :: Text -> VowelHarmony
vowelHarmony word =
  case any (== True) $ fmap (\c -> T.elem c word) backVowels of
    True -> BackHarmony
    _ -> FrontHarmony

elemFromList list x
  | elem x list = Just x
  | otherwise = Nothing

applySavoDiphthongNarrowing = modifyAccumulating diphthongNarrowingAcc
applyWesternDiphthongWidening word =
  modifyAccumulating (westernDiphthongWideningAcc $ vowelHarmony word) word

diphthongNarrowingAcc :: Text -> TextAcc
diphthongNarrowingAcc text =
  maybe
    (TextAcc 1 $ T.take 1 text)
    (TextAcc 2 . doNarrowing)
    (lookupTextNorm savoNarrowableDiphthongs $ T.take 2 text) where
  -- 'e' gets the case of the second element
  doNarrowing text = T.take 1 text <> applyCase (toLetterCase $ takeEnd 1 text) "e"

westernDiphthongWideningAcc :: VowelHarmony -> Text -> TextAcc
westernDiphthongWideningAcc harmony text =
  maybe
    (TextAcc 1 $ T.take 1 text)
    -- Case of the first letter is used
    (TextAcc 2 . doWidening harmony (toLetterCase $ T.take 1 text))
    (lookupTextNormHaystack westernWideningDiphthongs $ T.take 2 text) where
  doWidening :: VowelHarmony -> LetterCase -> Text -> Text
  doWidening FrontHarmony c "ie" = applyCase c "iä"
  doWidening BackHarmony c "ie" = applyCase c "ia"
  doWidening _ c "uo" = applyCase c "ua"
  doWidening _ c "yö" = applyCase c "yä"
  doWidening _ _ x = x
