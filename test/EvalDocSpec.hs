module EvalDocSpec (spec) where

import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.HashMap.Lazy as Map
import qualified EvalDoc as Eval
import Helpers (runDocParse')
import qualified ParseDoc as Parse
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.RawString.QQ (r)

spec :: Spec
spec = do
  describe "Evaluate documents" $ do
    ifTags
    expressTags
    it "Simple eval" $ do
      Eval.eval Map.empty (Parse.Doc (Aeson.KeyMap.fromList [("key", "value")]) [])
        `shouldBe` (Map.fromList [(["key"], Eval.Str "value")], "")
      Eval.eval
        Map.empty
        (Parse.Doc Aeson.KeyMap.empty [Parse.Cont "blah", Parse.Cont "blah"])
        `shouldBe` (Map.empty, "blahblah")

expressTags :: Spec
expressTags = do
  describe "Evaluate '{{}}' express tags" $ do
    it "display strings" $ do
      let doc = runDocParse' [r|{{ "hey" }}, {{ x }}|]
      let env = Map.fromList [(["x"], Eval.Str "blah")]
      Eval.eval env doc `shouldBe` (env, "hey, blah")

    it "display arrays" $ do
      let doc = runDocParse' "{{ the_array }}"
      let env = Map.fromList [(["the_array"], Eval.Array [Eval.Str "one", Eval.Str "two"])]
      Eval.eval env doc `shouldBe` (env, "[one, two]")

    it "display maps" $ do
      let doc = runDocParse' "{{ the_map }}"
      let env =
            Map.fromList
              [
                ( ["the_map"]
                , Eval.Map
                    ( Map.fromList
                        [ (["key"], Eval.Str "value")
                        ,
                          ( ["next_map"]
                          , Eval.Map
                              (Map.fromList [(["key"], Eval.Str "value")])
                          )
                        ]
                    )
                )
              ]
      Eval.eval env doc `shouldBe` (env, "{ key: value, next_map: { key: value } }")

    it "display booleans" $ do
      let doc = runDocParse' "{{ true }} is not {{ x }}"
      let env = Map.fromList [(["x"], Eval.Bool False)]
      Eval.eval env doc `shouldBe` (env, "true is not false")

ifTags :: Spec
ifTags = do
  describe "Evaluate 'if' tags" $ do
    it "'if' tag (executes)" $ do
      let doc =
            runDocParse'
              [r|
{%- if foo.bar -%}
  {{ foo.bar }}
{%- endif -%}
|]
      let env = Map.fromList [(["foo", "bar"], Eval.Str "baz")]
      Eval.eval env doc `shouldBe` (env, "baz")

    it "'if' tag (doesn't execute)" $ do
      let doc = runDocParse' "{%- if false -%} not funny : ( {%- endif -%}"
      Eval.eval Map.empty doc `shouldBe` (Map.empty, "")

    it "'if' tag with else" $ do
      let doc =
            runDocParse'
              [r|
{%- if false -%}
  False is true?
{%- else -%}
  False is False!
{%- endif -%}
|]
      Eval.eval Map.empty doc `shouldBe` (Map.empty, "False is False!")

    it "'if' tag with elsif" $ do
      let doc =
            runDocParse'
              [r|
{%- if false -%}
  Won't happen ...
{%- elsif blah -%}
  Maybe this happens?
{%- elsif foo -%}
  No, really this happens!
{%- endif -%}|]
      let env = Map.fromList [(["blah"], Eval.Nil), (["foo"], Eval.Bool True)]
      Eval.eval env doc `shouldBe` (env, "No, really this happens!")

    it "'if' with elsif and else" $ do
      let doc =
            runDocParse'
              [r|
{%- if false -%}
  Won't happen ...
{%- elsif blah -%}
  Maybe this happens?
{%- elsif foo -%}
  Now this doesn't happen either.
{%- else -%}
  Oh, at last!
{%- endif -%}|]
      let env = Map.fromList [(["blah"], Eval.Nil), (["foo"], Eval.Bool False)]
      Eval.eval env doc `shouldBe` (env, "Oh, at last!")
