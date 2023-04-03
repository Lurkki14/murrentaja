{-# LANGUAGE OverloadedStrings #-}

module Epenthesis
  (applyEpenthesis, applyPohjanmaaEpenthesis) where

import Data.Char
import Data.Maybe
import Data.Text hiding (elem, length, take)
import qualified Data.Text as T

import Utils

newtype Vowel = Vowel Char
newtype EpenthesisTrigger = EpenthesisTrigger Text

mkVowel :: Char -> Maybe Vowel 
mkVowel x
  | elem x vowels = Just $ Vowel x
  | otherwise = Nothing

mkEpenthesisTrigger :: [Text] -> Text -> Maybe EpenthesisTrigger
mkEpenthesisTrigger triggers x
  | elem x triggers = Just $ EpenthesisTrigger x
  | otherwise = Nothing

vowels = [ 'a', 'e', 'i', 'u', 'o', 'y', 'ä', 'ö' ] :: [Char]

epenthesisTriggers = [ "lp", "lk", "lm", "lv", "nh" ] :: [Text]
pohjanmaaEpenthesisTriggers = epenthesisTriggers ++ [ "hr", "hm", "hl", "hj", "hn", "hv" ]

applyEpenthesis = modifyAccumulating $ epenthesisAccWithTriggers epenthesisTriggers
applyPohjanmaaEpenthesis =
  modifyAccumulating $ epenthesisAccWithTriggers pohjanmaaEpenthesisTriggers

epenthesisAccWithTriggers :: [Text] -> Text -> TextAcc
epenthesisAccWithTriggers triggers text =
  fromMaybe
    (TextAcc 1 $ T.take 1 text)
    (uncons text >>= \(char, tail) ->
      doEpenthesis (mkVowel char) (mkEpenthesisTrigger triggers (T.take 2 tail)) >>= \ep ->
        pure $ TextAcc 3 ep) where -- 3, eg. (length trigger) + 1
    doEpenthesis (Just (Vowel v)) (Just (EpenthesisTrigger trigger)) =
      Just $ singleton v `append` T.take 1 trigger `append`
        singleton v `append` T.drop 1 trigger
    doEpenthesis _ _ = Nothing
