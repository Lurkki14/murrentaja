import Data.Text
import qualified Data.Text as T
import Test.QuickCheck
import Test.QuickCheck.Instances

import Murrentaja

f = fromFeatures [CommonGemination]

-- Very inefficient (should generate just letters) but works somewhat...
prop_CasesSame text
  | T.toLower text == T.toLower (T.toUpper text) = go text
  | otherwise = True where
      go text = modToLower text == modToLower (T.toUpper text)
      modToLower = fmap toLower . transform f

main = verboseCheckResult prop_CasesSame
