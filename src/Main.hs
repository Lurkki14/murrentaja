{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Char as T
import Data.Maybe
import Data.Text.Encoding
import Data.Text hiding (foldr, zip)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Options.Applicative

import Gemination

-- Program options
data Input =
  File FilePath |
  StdIn |
  Interactive deriving (Show)

-- Language features
-- Associate all these with LangWord -> Maybe Text (Maybe because replacing is expensive)
-- We also might need to model dependencies and conflicts
data PhonologicalFeatures = PhonologicalFeatures {
  commonGemination :: Bool,
  specialGemination :: Bool,
  epenthesis :: Bool -- https://kaino.kotus.fi/visk/sisallys.php?p=33
}

input :: Parser Input
input = file <|> stdIn <|> interactive where
  file = File <$> strOption (
    long "file" <>
    short 'f' )
  stdIn = flag' StdIn (
    long "stdin" <>
    short 's' )
  interactive = flag' Interactive (
    long "interactive" <>
    short 'i' )

options :: ParserInfo Input
options = info (input <**> helper) mempty

-- TODO: do other transformations here as well
replacements :: T.Text -> [(T.Text, T.Text)]
replacements text = [ (orig, gem) | (orig, Just gem) <- zip words geminated ] where
  words = T.words text
  geminated = fmap applyCommonGemination . parseCommonGeminable <$> words

-- TODO: rename to fit the goal above
commonGeminateText :: Text -> Text
commonGeminateText text =
  foldr (\(orig, gem) acc -> replace orig gem acc) text $ replacements text

interactiveLoop :: IO ()
interactiveLoop =
  T.IO.getLine >>= T.IO.putStrLn . ("-> " <>) . commonGeminateText >> interactiveLoop

main = do
  options <- execParser options
  doMain options where
    doMain Interactive = T.IO.putStrLn "Enter a line of text: " >> interactiveLoop
    doMain StdIn = pure () -- TODO: do something :D
    doMain (File filePath) = T.IO.readFile filePath >>= T.IO.putStr . commonGeminateText
