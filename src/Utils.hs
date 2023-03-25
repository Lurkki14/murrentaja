{-# LANGUAGE OverloadedStrings, OverloadedRecordDot #-}

module Utils where

import Data.Text hiding (drop)
import qualified Data.Text as T

data TextAcc = TextAcc {  
  originalLength :: Int,
  modified :: Text
} deriving (Show)

-- Captures the repeated pattern from other modifying functions
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
