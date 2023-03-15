{-# LANGUAGE OverloadedStrings #-}

module Epenthesis
  (applyEpenthesis) where

import Data.Char
import Data.Maybe
import Data.Text hiding (elem, length, take)
import qualified Data.Text as T

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

applyEpenthesis = epenthesisWithTriggers epenthesisTriggers

epenthesisWithTriggers :: [Text] -> Text -> Maybe Text
epenthesisWithTriggers triggers word
  | epenthesized == word = Nothing
  | otherwise = Just epenthesized where
  epenthesized = go triggers "" word
  go :: [Text] -> Text -> Text -> Text
  go _ acc "" = acc
  go triggers acc text =
    -- 'text' in go "j" "alka" needs to be cut by 3
    -- go "jalak" "a" by 1
    let nextAcc' = nextAcc triggers text
    in go triggers (append acc $ fst nextAcc') (T.drop (snd nextAcc') text)
  nextAcc :: [Text] -> Text -> (Text, Int)
  nextAcc triggers text =
    fromMaybe
      ((,) (T.take 1 text) 1)
      (uncons text >>= \(char, tail) ->
        doEpenthesis (mkVowel char) (mkEpenthesisTrigger triggers (T.take 2 tail)) >>= \ep ->
          pure (ep, 3)) -- 3, eg. (length trigger) + 1
  doEpenthesis (Just (Vowel v)) (Just (EpenthesisTrigger trigger)) =
    Just $ singleton v `append` T.take 1 trigger `append`
      singleton v `append` T.drop 1 trigger
  doEpenthesis _ _ = Nothing
