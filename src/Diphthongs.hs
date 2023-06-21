{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Diphthongs
  (applySavoDiphthongNarrowing, applyWesternDiphthongWidening) where

import Data.Char
import Data.Maybe
import Data.Text hiding (any, elem, drop)
import qualified Data.Text as T

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
applyWesternDiphthongWidening = modifyAccumulating westernDiphthongWideningAcc

diphthongNarrowingAcc :: Text -> TextAcc
diphthongNarrowingAcc text =
  maybe
    (TextAcc 1 $ T.take 1 text)
    (TextAcc 2 . doNarrowing)
    (elemFromList savoNarrowableDiphthongs $ T.take 2 text) where
  doNarrowing text = T.take 1 text <> "e"

westernDiphthongWideningAcc :: Text -> TextAcc
westernDiphthongWideningAcc text =
  maybe
    (TextAcc 1 $ T.take 1 text)
    (TextAcc 2 . doWidening harmony)
    (elemFromList westernWideningDiphthongs $ T.take 2 text) where
  harmony = vowelHarmony text
  doWidening :: VowelHarmony -> Text -> Text
  doWidening FrontHarmony "ie" = "iä"
  doWidening _ "uo" = "ua"
  doWidening _ "yö" = "yä"
  doWidening _ x = x
