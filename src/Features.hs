{-# LANGUAGE OverloadedStrings #-}

module Features where

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

mkEpenthesisTrigger :: Text -> Maybe EpenthesisTrigger
mkEpenthesisTrigger x
  | elem x epenthesisTriggers = Just $ EpenthesisTrigger x
  | otherwise = Nothing

vowels = [ 'a', 'e', 'i', 'u', 'o', 'y', 'ä', 'ö' ] :: [Char]

epenthesisTriggers = [ "lp", "lk", "lm", "lv", "nh" ] :: [Text]

applyEpenthesis :: Text -> Maybe Text
applyEpenthesis word
  | epenthesized == word = Nothing
  | otherwise = Just epenthesized where
  epenthesized = go "" word
  go :: Text -> Text -> Text
  go acc "" = acc
  go acc text =
    -- go "j" "alka" needs to be cut by 3
    -- go "jalak" "a" by 1
    go (append acc $ fst $ nextAcc text) (T.drop (snd $ nextAcc text) text)
  nextAcc :: Text -> (Text, Int)
  nextAcc text =
    fromMaybe
      ((,) (T.take 1 text) 1)
      (uncons text >>= \(char, tail) ->
        doEpenthesis (mkVowel char) (mkEpenthesisTrigger (T.take 2 tail)) >>= \ep ->
          pure (ep, 3)) -- 3, eg. (length trigger) + 1
  doEpenthesis (Just (Vowel v)) (Just (EpenthesisTrigger trigger)) =
    Just $ singleton v `append` T.take 1 trigger `append`
      singleton v `append` T.drop 1 trigger
  doEpenthesis _ _ = Nothing
