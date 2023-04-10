{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Diphthongs
  (applySavoReduction, applyWesternReduction) where

import Data.Char
import Data.Maybe
import Data.Text hiding (any, elem, drop)
import qualified Data.Text as T

import Utils

data VowelHarmony =
  BackHarmony |
  FrontHarmony deriving (Show)

-- Cause eg. poika -> poeka
reducibleDiphthongs = [ "oi", "öi", "ai", "äi" ] :: [Text]
-- työmies -> tyämiäs
westernReducibleDiphthongs = [ "ie", "uo", "yö" ] :: [Text]

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

applySavoReduction = modifyAccumulating diphthongReductionAcc
applyWesternReduction = modifyAccumulating westernDiphthongReductionAcc

diphthongReductionAcc :: Text -> TextAcc
diphthongReductionAcc text =
  maybe
    (TextAcc 1 $ T.take 1 text)
    (TextAcc 2 . doReduction)
    (elemFromList reducibleDiphthongs $ T.take 2 text) where
  doReduction text = T.take 1 text <> "e"

westernDiphthongReductionAcc :: Text -> TextAcc
westernDiphthongReductionAcc text =
  maybe
    (TextAcc 1 $ T.take 1 text)
    (TextAcc 2 . doReduction harmony)
    (elemFromList westernReducibleDiphthongs $ T.take 2 text) where
  harmony = vowelHarmony text
  doReduction :: VowelHarmony -> Text -> Text
  doReduction FrontHarmony "ie" = "iä"
  doReduction _ "uo" = "ua"
  doReduction _ "yö" = "yä"
  doReduction _ x = x
