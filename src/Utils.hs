{-# LANGUAGE OverloadedStrings, OverloadedRecordDot #-}

module Utils where

import Data.Text hiding (drop)
import qualified Data.Text as T

data TextAcc = TextAcc {  
  originalLength :: Int,
  modified :: Text
} deriving (Show)

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
