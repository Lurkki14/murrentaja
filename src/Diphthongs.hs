{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Diphthongs
  (applySavoReduction) where

import Data.Maybe
import Data.Text hiding (elem, drop)
import qualified Data.Text as T

import Utils

-- Cause eg. poika -> poeka
reducibleDiphthongs = [ "oi", "öi", "ai", "äi" ] :: [Text]
-- työmies -> tyämiäs
westernReducibleDiphthongs = [ "ie", "uo", "yö" ] :: [Text]

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
    (TextAcc 2 . doReduction)
    (elemFromList westernReducibleDiphthongs $ T.take 2 text) where
  doReduction :: Text -> Text
  doReduction "ie" = "iä"
  doReduction "uo" = "ua"
  doReduction "yö" = "yä"
  doReduction _ = ""
