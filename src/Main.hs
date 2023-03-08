{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Char as T
import Data.Maybe
import Data.Text.Encoding
import Data.Text hiding (foldr, zip, words)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Options.Applicative
import Text.Read

import Gemination
import Epenthesis

-- Program options
data Input =
  File FilePath |
  StdIn |
  Interactive deriving (Show)

data Feature =
  CommonGemination |
  SpecialGemination |
  Epenthesis deriving (Read, Show)

data Options = Options {
  inputOpt :: Input,
  featuresOpt :: [Feature]
} deriving (Show)

data FeatureInfo = FeatureInfo {
  feature :: Feature,
  function :: Text -> Maybe Text,
  conflicts :: [Feature],
  supersetOf :: [Feature]
}

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

features :: Parser [Feature]
features = option parseFeatures (
  long "features" <>
  short 'F' ) where
    parseFeatures :: ReadM [Feature]
    parseFeatures = eitherReader readEither

readFeatures :: String -> [Maybe Feature]
readFeatures string = readMaybe <$> words string


optionsP :: Parser Options
optionsP = Options <$> input <*> features

options :: ParserInfo Options
options = info (optionsP <**> helper) mempty

transformations = [ commonGeminated, applyEpenthesis ] :: [Text -> Maybe Text]

transformationsPlain = fmap (ap fromMaybe) transformations :: [Text -> Text]

thread :: Foldable t => t (a -> a) -> a -> a
thread = foldr (.) id

-- Return Maybe Text so we don't try to replace every word,
-- whether they've been modified or not
transform :: Text -> Maybe Text
transform word
  | transformed == word = Nothing
  | otherwise = Just transformed where
    transformed = thread transformationsPlain word

replacements :: Text -> [(Text, Text)]
replacements text = [ (orig, mod) | (orig, Just mod) <- zip words transformed ] where
  words = T.words text
  transformed = fmap transform words

transformText :: Text -> Text
transformText text =
  foldr (\(orig, gem) acc -> replace orig gem acc) text $ replacements text

interactiveLoop :: IO ()
interactiveLoop =
  T.IO.getLine >>= T.IO.putStrLn . ("-> " <>) . transformText >> interactiveLoop

main = do
  options <- execParser options
  doMain options.inputOpt where
    doMain Interactive = T.IO.putStrLn "Enter a line of text: " >> interactiveLoop
    doMain StdIn = pure () -- TODO: do something :D
    doMain (File filePath) = T.IO.readFile filePath >>= T.IO.putStr . transformText
