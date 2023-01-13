{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

-- Other vowel combinations aren't considered diphthongs and will break a syllable (eg. ta-e)
diphthongs = [ "ei", "öi", "äi", "oi", "ai", "ey", "öy",
  "äy", "eu", "ou", "au", "yi", "ui", "iy", "iu" ] :: [T.Text] 

parseDiphthong :: T.Text -> Maybe T.Text
parseDiphthong x
  | T.length candidate /= 2 = Nothing
  | not $ elem candidate diphthongs = Nothing
  | elem candidate diphthongs = Just x
  where candidate = T.take 2 x -- Text.take is O(n) so don't waste time calculating the length of a big string

-- Splitting into syllables
-- 1. Add all initial consonants to syllable (psal-mi, stres-si)
-- 2. If next two characters constitute a diphthong, the syllable is complete
-- 3. Same goes for a long vowel

main = do
  input <- T.IO.getContents
  T.IO.putStrLn $ fromMaybe "Not a diphthong" (parseDiphthong input)
