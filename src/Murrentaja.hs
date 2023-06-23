{-# LANGUAGE OverloadedRecordDot #-}

module Murrentaja where

import Control.Applicative
import Control.Monad
import Data.Map.Strict hiding (foldr, filter, lookup, singleton)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as Set
import Data.Text hiding (foldr, zip)
import qualified Data.Text as T

import Gemination
import Epenthesis
import Diphthongs
import Lenition


data Feature =
  CommonGemination |
  SpecialGemination |
  Epenthesis |
  PohjanmaaEpenthesis |
  SavoDiphthongNarrowing |
  WesternDiphthongWidening |
  SWDLenition deriving (Eq, Ord, Read, Show)

type Transformation = (Text -> Text)

-- Geographical presets
data Preset =
  Kuopio |
  Tampere |
  Oulu deriving (Enum, Read, Show)

data FeatureInfo = FeatureInfo {
  feature :: Feature,
  function :: Text -> Maybe Text,
  -- Throw error if one of these appears in the same list
  -- Or: valid list can't be created if in same list
  conflicts :: [Feature],
  -- Any instance of these is removed if in the same list
  supersetOf :: [Feature]
}

fromPreset :: Preset -> [Feature]
fromPreset Kuopio = [SpecialGemination, Epenthesis, SavoDiphthongNarrowing]
fromPreset Tampere = [WesternDiphthongWidening, SWDLenition]
-- Western uo -> ua is common around Oulu but not Oulu itself?
fromPreset Oulu = [CommonGemination, PohjanmaaEpenthesis]

featureInfo :: [FeatureInfo]
featureInfo =
  [
    FeatureInfo CommonGemination commonGeminated [] [],
    FeatureInfo Epenthesis applyEpenthesis [] [],
    FeatureInfo SpecialGemination applySpecialGemination [] [CommonGemination],
    FeatureInfo PohjanmaaEpenthesis applyPohjanmaaEpenthesis [] [Epenthesis],
    FeatureInfo SavoDiphthongNarrowing applySavoDiphthongNarrowing [] [],
    FeatureInfo WesternDiphthongWidening applyWesternDiphthongWidening [] [],
    FeatureInfo SWDLenition applySWDLention [] []
  ]

thread :: Foldable t => t (a -> a) -> a -> a
thread = foldr (.) id

-- Return Maybe Text so we don't try to replace every word,
-- whether they've been modified or not
transform :: [Transformation] -> Text -> Maybe Text
transform fs word
  | transformed == word = Nothing
  | otherwise = Just transformed where
    transformed = thread fs word

replacements :: [Transformation] -> Text -> [(Text, Text)]
replacements fs text = [ (orig, mod) | (orig, Just mod) <- zip words transformed ] where
  words = T.words text
  transformed = fmap (transform fs) words

transformText :: [Transformation] -> Text -> Text
transformText fs text =
  foldr (\(orig, mod) acc -> replace orig mod acc) text $ replacements fs text

-- TODO: make more debuggable by making a function [Feature] -> [Feature]
fromFeatures :: [Feature] -> [Transformation]
fromFeatures features =
  (\(k, featureInfo) -> ap fromMaybe featureInfo.function) <$>
    M.toList withoutSubsets where
      mapWithFeatures = M.restrictKeys featureInfoMap $ Set.fromList features
      withoutSubsets =
        foldr (\feature acc -> removeSubsets acc feature) mapWithFeatures features
      removeSubsets :: Map Feature FeatureInfo -> Feature -> Map Feature FeatureInfo
      removeSubsets map feature = fromMaybe map $ M.lookup feature map >>= \v ->
        pure $ foldr delete map v.supersetOf
      featureInfoMap = fromList $ fmap (\info -> (,) info.feature info) featureInfo
      -- TODO: remove conflicting features
