module EvalDocSpec (spec) where

import Test.Hspec ( Spec, describe, it, shouldBe )
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.HashMap.Lazy as Map

import qualified ParseDoc as Parse
import qualified EvalDoc as Eval

spec :: Spec
spec = do
  describe "Evaluate documents" $ do
    it "Simple eval" $ do
      Eval.eval Map.empty (Parse.Doc (Aeson.KeyMap.fromList [("key", "value")]) [])
        `shouldBe`
          (Map.fromList [("key", Eval.Str "value")], "")
      Eval.eval Map.empty (Parse.Doc Aeson.KeyMap.empty [Parse.Cont "blah", Parse.Cont "blah"])
        `shouldBe`
          (Map.empty, "blahblah")
