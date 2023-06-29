{-# LANGUAGE OverloadedStrings,
             OverloadedRecordDot,
             ConstraintKinds,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts #-}

module Utils where

import Data.Char hiding (toLower, toUpper)
import Data.Map.Strict hiding (lookup)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text hiding (drop, elem)
import qualified Data.Text as T
import Prelude hiding (lookup)

-- Functional dependency might work better
class Ord a => Searchable b a where
  lookup :: b a -> a -> Maybe a

instance Ord a => Searchable [] a where
  lookup list x = lookupElem list x where
    lookupElem list x
      | elem x list = Just x
      | otherwise = Nothing

data TextAcc = TextAcc {  
  originalLength :: Int,
  modified :: Text
} deriving (Show)

data LetterCase = LowerCase | UpperCase deriving (Show)

-- Shouldn't matter that no checks are performed here for our purposes,
-- when nonsense like numbers get passed, that result shouldn't get used
letterCase :: Char -> LetterCase
letterCase x
  | isLower x = LowerCase
  | otherwise = UpperCase -- will be the rarer case

toLetterCase :: Text -> LetterCase
toLetterCase x =
  fromMaybe
    LowerCase
    (uncons x >>= \(c, _) -> pure $ letterCase c)

applyCase :: LetterCase -> Text -> Text
applyCase LowerCase x = toLower x
applyCase UpperCase x = toUpper x

-- Is FlexibleContexts really needed for this?
lookupTextNorm :: Searchable b Text => b Text -> Text -> Maybe Text
lookupTextNorm xs element = lookup xs (toCaseFold element) >> Just element

-- Captures the repeated pattern from all earlier modifying functions
-- This function allows modifying words at the precision required for
-- different transformations, and allows for the modified text to be of
-- different length by allowing functions to specify how the modified
-- text maps to the original word

-- TODO: this function should split compound words into their elements
-- while processing for correctness
modifyAccumulating :: (Text -> TextAcc) -> Text -> Maybe Text
modifyAccumulating f word
  | modified == word = Nothing
  | otherwise = Just modified where
  modified = go "" word
  go :: Text -> Text -> Text
  go acc "" = acc
  go acc text =
    let nextAcc = f text
    in go (append acc nextAcc.modified) $ T.drop nextAcc.originalLength text
