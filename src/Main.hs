{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Encoding
import Data.Text hiding (head, foldr, foldr1, zip, words)
import qualified Data.Text.IO as T.IO
import Options.Applicative
import Options.Applicative.Help.Pretty hiding (empty, group)
import qualified Options.Applicative.Help.Pretty as P
import Text.Read

import Murrentaja

-- Program options
data Input =
  File FilePath |
  StdIn |
  Interactive deriving (Show)

-- TODO: allow preset and specifying features at the same time
data FeatureInput =
  Features [Feature] |
  Preset Preset deriving (Read, Show)

data Options = Options {
  inputOpt :: Input,
  featuresOpt :: FeatureInput
} deriving (Show)

showCommaSep :: Show a => [a] -> String
showCommaSep [] = ""
showCommaSep [x] = show x
showCommaSep (x:xs) = show x <> ", " <> showCommaSep xs

instance Pretty FeatureInfo where
  pretty FeatureInfo {
      feature,
      conflicts,
      supersetOf
    } = P.group $ nest 2 (featureDoc .$. supersetDoc supersetOf) where
      featureDoc = text $ show feature
      supersetDoc [] = P.empty
      supersetDoc x = text $ "Implies: " <> showCommaSep supersetOf
  prettyList [] = P.empty
  -- Print each FeatureInfo on its own line
  prettyList f_infos = foldr1 (<$$>) $ fmap pretty f_infos

instance Pretty Preset where
  pretty = text . show
  prettyList [] = P.empty
  prettyList presets = foldr1 (<$$>) $ fmap pretty presets

input :: Parser Input
input = file <|> stdIn <|> interactive where
  file = File <$> strOption (
    long "file" <>
    short 'f' <>
    help "Read text from FILE" <>
    metavar "FILE")
  stdIn = flag' StdIn (
    long "stdin" <>
    short 's' <>
    help "Read text from stdin")
  interactive = flag' Interactive (
    long "interactive" <>
    short 'i' <>
    help "Read text from an interactive prompt")

features :: Parser FeatureInput
features = Features <$> features' <|> Preset <$> preset where
  preset :: Parser Preset
  preset = option auto (
    long "preset" <>
    short 'p' <>
    metavar "PRESET" <>
    helpDoc
      (Just $ "Geographical preset, available ones:" <$$>
        prettyList (enumFrom (toEnum 0 :: Preset))))
  features' :: Parser [Feature]
  features' = option parseFeatures (
    long "features" <>
    short 'F' <>
    metavar "FEATURES" <>
    --helpDoc fhelp) where
    helpDoc
      (Just $ "Comma separated list of features, available ones:" <$$>
      prettyList featureInfo)) where
      fhelp = Just $ nest 2 $ text "hello" .$. text "world"
      parseFeatures :: ReadM [Feature]
      parseFeatures = eitherReader readFeaturesE
      readFeaturesE :: String -> Either String [Feature]
      readFeaturesE string =
        case readFeatures string of (Just xs) -> Right xs
                                    Nothing -> Left "No such feature name"
      -- Parse a comma separated list of Features
      readFeatures :: String -> Maybe [Feature]
      readFeatures string = mapM readMaybe strings where
        strings :: [String]
        strings = fmap unpack $ splitOn "," $ pack string

optionsP :: Parser Options
optionsP = Options <$> input <*> features

options :: ParserInfo Options
options = info (optionsP <**> helper) mempty

interactiveLoop :: Options -> IO ()
interactiveLoop opts =
  T.IO.getLine >>=
    T.IO.putStrLn . ("-> " <>) . transformText (featureFuncs opts.featuresOpt) >>
      interactiveLoop opts

featureFuncs (Features xs) = fromFeatures xs
featureFuncs (Preset p) = fromFeatures $ fromPreset p

main = do
  options <- execParser options
  doMain options where
    doMain :: Options -> IO ()
    doMain options@Options {inputOpt = Interactive} =
      T.IO.putStrLn "Enter a line of text: " >>
        interactiveLoop options
    doMain Options {inputOpt = StdIn, featuresOpt} =
      T.IO.getContents >>= T.IO.putStr . transformText (featureFuncs featuresOpt)
    doMain Options {inputOpt = (File filePath), featuresOpt} =
      T.IO.readFile filePath >>= T.IO.putStr . transformText (featureFuncs featuresOpt)
