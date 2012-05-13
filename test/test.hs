import qualified Data.Vector as V
import Test.Hspec.Monadic
import Test.Hspec.HUnit

import Data.Liblinear

main = hspecX $ do
  describe "liblinear" $ do
    it "correct" $ do
      let prob = Problem
            { problemData = V.empty
            , problemBias = 0
            }
      model <- train prob def
      return ()
