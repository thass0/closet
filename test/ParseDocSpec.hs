module ParseDocSpec (spec) where

import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import Data.Either (isLeft)
import qualified Data.Yaml as Y
import Helpers (doc, runDocParse)
import qualified ParseDoc
import Test.Hspec
import Text.RawString.QQ (r)

-- * Tests

spec :: Spec
spec = do
  describe "Parse documents" $ do
    frontMatter
    expressions
    tags

    it "Copy white space at start of input" $ do
      runDocParse "   This is my entire document! "
        `shouldBe` Right
          (doc [ParseDoc.Cont "   This is my entire document! "])

-- ** Tests on parsing the front matter

frontMatter :: Spec
frontMatter = do
  describe "Parse front matter" $ do
    it "Basic front matter" $ do
      let input =
            [r|
---
name: "This is my name"
age: 41
children:
    - Child1
    - Child2
    - Child3
---
This is the content of this page!|]
      runDocParse input
        `shouldBe` ( Right $
                      ParseDoc.Doc
                        { ParseDoc.docFrontMatter =
                            Aeson.KeyMap.fromList
                              [ ("name", Y.String "This is my name")
                              , ("age", Y.Number 41)
                              , ("children", Y.array ["Child1", "Child2", "Child3"])
                              ]
                        , ParseDoc.docBlocks = [ParseDoc.Cont "This is the content of this page!"]
                        }
                   )

    it "Allow \"---\" inside the front matter" $ do
      let input =
            [r|
---
title: "Blah --- my personal blog about Blah!"
---
Blah is the best filler word possible.|]
      runDocParse input
        `shouldBe` ( Right $
                      ParseDoc.Doc
                        { ParseDoc.docFrontMatter =
                            Aeson.KeyMap.fromList
                              [("title", Y.String "Blah --- my personal blog about Blah!")]
                        , ParseDoc.docBlocks = [ParseDoc.Cont "Blah is the best filler word possible."]
                        }
                   )

    it "Closing \"---\" must be at beginning of line" $ do
      let input1 = "---\r\ntitle: My blog ---\r\n"
      runDocParse input1 `shouldSatisfy` isLeft
      let input2Win = "---\r\ntitle: My blog\r\n---\r\n"
      let input2Unix = "---\ntitle: My blog\n---\n"
      let input2Doc =
            ParseDoc.Doc
              { ParseDoc.docFrontMatter = Aeson.KeyMap.fromList [("title", Y.String "My blog")]
              , ParseDoc.docBlocks = [ParseDoc.Cont ""]
              }
      runDocParse input2Win `shouldBe` Right input2Doc
      runDocParse input2Unix `shouldBe` Right input2Doc

    it "Commit to front matter after seeing a single \"---\"" $ do
      -- If the file contains a valid front matter start, the parser may not
      -- fall back to parsing this part of the file as normal file content.
      -- If the front matter that follows is invalid, the parser must fail entirely.
      let input = "---\ntitle: This blog has a front matter that does not end.\n"
      runDocParse input `shouldSatisfy` isLeft

-- ** Tests on parsing expressions

expressions :: Spec
expressions = do
  describe "Parse expressions" $ do
    filters
    booleanExpressions

booleanExpressions :: Spec
booleanExpressions = do
  describe "Boolean expressions" $ do
    it "'and' and 'or' are right-associative" $ do
      parseCondition "a and b or c"
        `shouldBe` parsedCondition
          ( ParseDoc.Expr
              ( ParseDoc.ExprAnd
                  (ParseDoc.ImmVar ["a"])
                  (ParseDoc.ExprOr (ParseDoc.ImmVar ["b"]) (ParseDoc.ImmVar ["c"]))
              )
              []
          )
      parseCondition "a or b and c or d"
        `shouldBe` parsedCondition
          ( ParseDoc.Expr
              ( ParseDoc.ExprOr
                  (ParseDoc.ImmVar ["a"])
                  ( ParseDoc.ExprAnd
                      (ParseDoc.ImmVar ["b"])
                      ( ParseDoc.ExprOr
                          (ParseDoc.ImmVar ["c"])
                          (ParseDoc.ImmVar ["d"])
                      )
                  )
              )
              []
          )

    it "'==' and '>' have higher precedence than 'and'" $ do
      parseCondition "a == b and c > d"
        `shouldBe` parsedCondition
          ( ParseDoc.Expr
              ( ParseDoc.ExprAnd
                  ( ParseDoc.ExprEq
                      (ParseDoc.ImmVar ["a"])
                      (ParseDoc.ImmVar ["b"])
                  )
                  ( ParseDoc.ExprGt
                      (ParseDoc.ImmVar ["c"])
                      (ParseDoc.ImmVar ["d"])
                  )
              )
              []
          )

    it "'<=' and '!=' have higher precedence than 'or'" $ do
      parseCondition "a <= b or c != d"
        `shouldBe` parsedCondition
          ( ParseDoc.Expr
              ( ParseDoc.ExprOr
                  ( ParseDoc.ExprLeq
                      (ParseDoc.ImmVar ["a"])
                      (ParseDoc.ImmVar ["b"])
                  )
                  ( ParseDoc.ExprNeq
                      (ParseDoc.ImmVar ["c"])
                      (ParseDoc.ImmVar ["d"])
                  )
              )
              []
          )

    it "'contains' and '<' have higher precedence than 'and'" $ do
      parseCondition "a contains b and 6 < 4"
        `shouldBe` parsedCondition
          ( ParseDoc.Expr
              ( ParseDoc.ExprAnd
                  ( ParseDoc.ExprContains
                      (ParseDoc.ImmVar ["a"])
                      (ParseDoc.ImmVar ["b"])
                  )
                  ( ParseDoc.ExprLt
                      (ParseDoc.ImmNum 6)
                      (ParseDoc.ImmNum 4)
                  )
              )
              []
          )

    it "Cannot chain operators without 'and' or 'or'" $ do
      parseCondition "a == b == c" `shouldSatisfy` isLeft
      parseCondition "a contains b >= c" `shouldSatisfy` isLeft

    it "'and'" $
      parseCondition "a and b"
        `shouldBe` parsedCondition
          ( ParseDoc.Expr
              (ParseDoc.ExprAnd (ParseDoc.ImmVar ["a"]) (ParseDoc.ImmVar ["b"]))
              []
          )

    it "'or'" $
      parseCondition "a or b"
        `shouldBe` parsedCondition
          ( ParseDoc.Expr
              (ParseDoc.ExprOr (ParseDoc.ImmVar ["a"]) (ParseDoc.ImmVar ["b"]))
              []
          )

    it "'=='" $
      parseCondition "a == b"
        `shouldBe` parsedCondition
          ( ParseDoc.Expr
              (ParseDoc.ExprEq (ParseDoc.ImmVar ["a"]) (ParseDoc.ImmVar ["b"]))
              []
          )

    it "'!='" $
      parseCondition "a != b"
        `shouldBe` parsedCondition
          ( ParseDoc.Expr
              (ParseDoc.ExprNeq (ParseDoc.ImmVar ["a"]) (ParseDoc.ImmVar ["b"]))
              []
          )

    it "'>'" $
      parseCondition "a > b"
        `shouldBe` parsedCondition
          ( ParseDoc.Expr
              (ParseDoc.ExprGt (ParseDoc.ImmVar ["a"]) (ParseDoc.ImmVar ["b"]))
              []
          )

    it "'<'" $
      parseCondition "a < b"
        `shouldBe` parsedCondition
          ( ParseDoc.Expr
              (ParseDoc.ExprLt (ParseDoc.ImmVar ["a"]) (ParseDoc.ImmVar ["b"]))
              []
          )

    it "'>='" $
      parseCondition "a >= b"
        `shouldBe` parsedCondition
          ( ParseDoc.Expr
              (ParseDoc.ExprGeq (ParseDoc.ImmVar ["a"]) (ParseDoc.ImmVar ["b"]))
              []
          )

    it "'<='" $
      parseCondition "a <= true"
        `shouldBe` parsedCondition
          ( ParseDoc.Expr
              (ParseDoc.ExprLeq (ParseDoc.ImmVar ["a"]) (ParseDoc.ImmBool True))
              []
          )

    it "'contains'" $
      parseCondition "a contains b"
        `shouldBe` parsedCondition
          ( ParseDoc.Expr
              ( ParseDoc.ExprContains
                  (ParseDoc.ImmVar ["a"])
                  (ParseDoc.ImmVar ["b"])
              )
              []
          )
  where
    parseCondition con =
      runDocParse $ "{% if " <> con <> " %}Blah{% endif %}"
    parsedCondition e =
      Right $
        doc
          [ ParseDoc.Tag
              ( ParseDoc.TagIf
                  e
                  [ParseDoc.Cont "Blah"]
                  []
                  Nothing
              )
          ]

filters :: Spec
filters = do
  describe "Filtered expressions" $ do
    -- TODO: Move the tests for individual filters somewhere
    -- else, once another part of the front-end validates
    -- that the filters have implementations.
    describe "Numeric filters" $ do
      it "'plus'" $
        "plus: 4891"
          `filterShouldBe` ParseDoc.FilterExpr "plus" [ParseDoc.ImmNum 4891]
      it "'minus'" $
        "minus: 51"
          `filterShouldBe` ParseDoc.FilterExpr "minus" [ParseDoc.ImmNum 51]
      it "'times'" $
        "times: -642"
          `filterShouldBe` ParseDoc.FilterExpr "times" [ParseDoc.ImmNum (-642)]
      it "'divided_by'" $
        "divided_by: y"
          `filterShouldBe` ParseDoc.FilterExpr "divided_by" [ParseDoc.ImmVar ["y"]]
      it "'modulo'" $
        "modulo: site.x"
          `filterShouldBe` ParseDoc.FilterExpr "modulo" [ParseDoc.ImmVar ["site", "x"]]
      it "'at_least'" $ do
        "at_least: 19"
          `filterShouldBe` ParseDoc.FilterExpr "at_least" [ParseDoc.ImmNum 19]
      it "'at_most'" $ do
        "at_most: 19"
          `filterShouldBe` ParseDoc.FilterExpr "at_most" [ParseDoc.ImmNum 19]
      it "'abs'" $ do
        "abs" `filterShouldBe` ParseDoc.FilterExpr "abs" []
        runDocParse "{{ -32 | abs | abs|abs|abs  | abs}}"
          `shouldBe` parsedExpr (ParseDoc.ImmNum (-32)) (replicate 5 (ParseDoc.FilterExpr "abs" []))
      it "'ceil'" $ "ceil" `filterShouldBe` ParseDoc.FilterExpr "ceil" []
      it "'floor'" $ "floor" `filterShouldBe` ParseDoc.FilterExpr "floor" []
      it "'round'" $ "round" `filterShouldBe` ParseDoc.FilterExpr "round" []

    describe "Array and object filters" $ do
      it "'append'" $ do
        "append: \"World!\""
          `filterShouldBe` ParseDoc.FilterExpr "append" [ParseDoc.ImmStrLit "World!"]
      it "'concat'" $
        "concat: wowow"
          `filterShouldBe` ParseDoc.FilterExpr "concat" [ParseDoc.ImmVar ["wowow"]]
      it "'first'" $ "first" `filterShouldBe` ParseDoc.FilterExpr "first" []
      it "'last'" $ "last" `filterShouldBe` ParseDoc.FilterExpr "last" []
      it "'join'" $
        "join: blah"
          `filterShouldBe` ParseDoc.FilterExpr "join" [ParseDoc.ImmVar ["blah"]]
      it "'reverse'" $ "reverse" `filterShouldBe` ParseDoc.FilterExpr "reverse" []
      it "'sort'" $ "sort" `filterShouldBe` ParseDoc.FilterExpr "sort" []
      it "'sort_natural'" $
        "sort_natural" `filterShouldBe` ParseDoc.FilterExpr "sort_natural" []
      it "'map'" $
        "map: \"blah\""
          `filterShouldBe` ParseDoc.FilterExpr "map" [ParseDoc.ImmStrLit "blah"]
      it "'compact'" $ "compact" `filterShouldBe` ParseDoc.FilterExpr "compact" []
      it "'sum'" $ do
        "sum" `filterShouldBe` ParseDoc.FilterExpr "sum" []
        "sum: \"category\""
          `filterShouldBe` ParseDoc.FilterExpr "sum" [ParseDoc.ImmStrLit "category"]
      it "'uniq'" $ "uniq" `filterShouldBe` ParseDoc.FilterExpr "uniq" []
      it "'where'" $ do
        "where: \"x\""
          `filterShouldBe` ParseDoc.FilterExpr "where" [ParseDoc.ImmStrLit "x"]
        "where: blah, foo"
          `filterShouldBe` ParseDoc.FilterExpr
            "where"
            [ ParseDoc.ImmVar ["blah"]
            , ParseDoc.ImmVar ["foo"]
            ]

    describe "String filters" $ do
      it "'capitalize'" $
        "capitalize" `filterShouldBe` ParseDoc.FilterExpr "capitalize" []
      it "'upcase'" $ "upcase" `filterShouldBe` ParseDoc.FilterExpr "upcase" []
      it "'downcase'" $ "downcase" `filterShouldBe` ParseDoc.FilterExpr "downcase" []
      it "'lstrip'" $ "lstrip" `filterShouldBe` ParseDoc.FilterExpr "lstrip" []
      it "'rstrip'" $ "rstrip" `filterShouldBe` ParseDoc.FilterExpr "rstrip" []
      it "'prepend'" $
        "prepend: \"blah\""
          `filterShouldBe` ParseDoc.FilterExpr "prepend" [ParseDoc.ImmStrLit "blah"]
      it "'replace'" $
        "replace: the.string, \"blah\""
          `filterShouldBe` ParseDoc.FilterExpr
            "replace"
            [ ParseDoc.ImmVar ["the", "string"]
            , ParseDoc.ImmStrLit "blah"
            ]
      it "'replace_first'" $
        "replace_first: \"blah\" , page.title "
          `filterShouldBe` ParseDoc.FilterExpr
            "replace_first"
            [ ParseDoc.ImmStrLit "blah"
            , ParseDoc.ImmVar ["page", "title"]
            ]
      it "'remove'" $
        "remove: the.string"
          `filterShouldBe` ParseDoc.FilterExpr "remove" [ParseDoc.ImmVar ["the", "string"]]
      it "'remove_first'" $
        "remove_first: \"blah\""
          `filterShouldBe` ParseDoc.FilterExpr "remove_first" [ParseDoc.ImmStrLit "blah"]
      it "'size'" $ "size" `filterShouldBe` ParseDoc.FilterExpr "size" []
      it "'slice'" $ do
        "slice: 4"
          `filterShouldBe` ParseDoc.FilterExpr "slice" [ParseDoc.ImmNum 4]
        "slice: 5, 9"
          `filterShouldBe` ParseDoc.FilterExpr "slice" [ParseDoc.ImmNum 5, ParseDoc.ImmNum 9]
      it "'split'" $
        "split: \",\""
          `filterShouldBe` ParseDoc.FilterExpr "split" [ParseDoc.ImmStrLit ","]
      it "'strip'" $ "strip" `filterShouldBe` ParseDoc.FilterExpr "strip" []
      it "'strip_html'" $
        "strip_html" `filterShouldBe` ParseDoc.FilterExpr "strip_html" []
      it "'strip_newlines'" $
        "strip_newlines" `filterShouldBe` ParseDoc.FilterExpr "strip_newlines" []
      it "'escape'" $ "escape" `filterShouldBe` ParseDoc.FilterExpr "escape" []
      it "'escape_once'" $
        "escape_once" `filterShouldBe` ParseDoc.FilterExpr "escape_once" []
      it "'newline_to_br'" $
        "newline_to_br" `filterShouldBe` ParseDoc.FilterExpr "newline_to_br" []
      it "'truncate'" $ do
        "truncate: 42"
          `filterShouldBe` ParseDoc.FilterExpr "truncate" [ParseDoc.ImmNum 42]
        "truncate: 64 ,\"blah\""
          `filterShouldBe` ParseDoc.FilterExpr
            "truncate"
            [ ParseDoc.ImmNum 64
            , ParseDoc.ImmStrLit "blah"
            ]
      it "'truncatewords'" $ do
        "truncatewords: 259"
          `filterShouldBe` ParseDoc.FilterExpr "truncatewords" [ParseDoc.ImmNum 259]
        "truncatewords:49,my.ellipse"
          `filterShouldBe` ParseDoc.FilterExpr
            "truncatewords"
            [ ParseDoc.ImmNum 49
            , ParseDoc.ImmVar ["my", "ellipse"]
            ]
      it "'date'" $
        "date: \"%a, %b %d, %y\""
          `filterShouldBe` ParseDoc.FilterExpr "date" [ParseDoc.ImmStrLit "%a, %b %d, %y"]
      it "'url_decode'" $
        "url_decode" `filterShouldBe` ParseDoc.FilterExpr "url_decode" []
      it "'url_encode'" $
        "url_encode" `filterShouldBe` ParseDoc.FilterExpr "url_encode" []

    describe "Miscellaneous filters" $ do
      it "'default'" $
        "default: site.title"
          `filterShouldBe` ParseDoc.FilterExpr "default" [ParseDoc.ImmVar ["site", "title"]]
  where
    parsedExpr
      :: ParseDoc.BaseExpr
      -> [ParseDoc.FilterExpr]
      -> Either String ParseDoc.Doc
    parsedExpr imm f =
      Right
        ( doc
            [ ParseDoc.Tag
                (ParseDoc.TagExpress (ParseDoc.Expr imm f))
            ]
        )
    filterShouldBe filterString fil =
      runDocParse ("{{ x | " <> filterString <> " }}")
        `shouldBe` parsedExpr (ParseDoc.ImmVar ["x"]) [fil]

-- ** Tests on parsing tags

tags :: Spec
tags = do
  describe "Parse tags" $ do
    expressTags
    ifTags
    unlessTags

expressTags :: Spec
expressTags = do
  describe "Express tags" $ do
    it "Basic express tags" $ do
      runDocParse "{{ blah.x }}"
        `shouldBe` parsedImmExpr (ParseDoc.ImmVar ["blah", "x"])
      runDocParse "{{ true }}"
        `shouldBe` parsedImmExpr (ParseDoc.ImmBool True)
      runDocParse "{{ false }}"
        `shouldBe` parsedImmExpr (ParseDoc.ImmBool False)
      runDocParse "{{ blah.x.y.hello.world }}"
        `shouldBe` parsedImmExpr (ParseDoc.ImmVar ["blah", "x", "y", "hello", "world"])
      runDocParse "{{ \"Hello, world!\" }}"
        `shouldBe` parsedImmExpr (ParseDoc.ImmStrLit "Hello, world!")
      runDocParse "{{ 543 }}"
        `shouldBe` parsedImmExpr (ParseDoc.ImmNum 543)
      runDocParse "{{ -61 }}"
        `shouldBe` parsedImmExpr (ParseDoc.ImmNum (-61))
      runDocParse "{{blah}}"
        `shouldBe` parsedImmExpr (ParseDoc.ImmVar ["blah"])
    it "Invalid express tags" $ do
      runDocParse "{{ blah!x }}" `shouldSatisfy` isLeft
      runDocParse "{{ blah.x. }}" `shouldSatisfy` isLeft
      runDocParse "{{ blah * 91 }}" `shouldSatisfy` isLeft
      runDocParse "{{ 514blah }}" `shouldSatisfy` isLeft
    it "Strip away white space around express tags" $ do
      runDocParse " foo {{- blah -}} bar "
        `shouldBe` Right
          ( doc
              [ ParseDoc.Cont " foo"
              , ParseDoc.Tag (ParseDoc.TagExpress (ParseDoc.Expr (ParseDoc.ImmVar ["blah"]) []))
              , ParseDoc.Cont "bar "
              ]
          )
  where
    parsedImmExpr imm =
      Right
        (doc [ParseDoc.Tag (ParseDoc.TagExpress (ParseDoc.Expr imm []))])

unlessTags :: Spec
unlessTags = do
  describe "Parse 'unless' tags" $ do
    -- NOTE: 'if' and 'unless' tags are the same under the hood.
    -- So, as long as this holds, it's OK if 'unless' doesn't has
    -- so many tests.
    it "Basic 'unless' tags" $ do
      let input =
            [r|
{%- unless blah.x -%}
  Hey, how are you?
{% elsif false %}
  I cannot think of content for this test!
{%- else -%}
  Same problem as before
{%endunless-%}
|]
      runDocParse input
        `shouldBe` parsedUnlessTag
          (ParseDoc.ImmVar ["blah", "x"])
          [ParseDoc.Cont "Hey, how are you?\n"]
          [
            ( ParseDoc.ImmBool False
            , [ParseDoc.Cont "\n  I cannot think of content for this test!"]
            )
          ]
          (Just [ParseDoc.Cont "\n  Same problem as before\n"])
  where
    parsedUnlessTag
      :: ParseDoc.BaseExpr
      -> [ParseDoc.Block]
      -> [(ParseDoc.BaseExpr, [ParseDoc.Block])]
      -> Maybe [ParseDoc.Block]
      -> Either String ParseDoc.Doc
    parsedUnlessTag prd' conseq alts' final =
      let alts = (\(e, b) -> (ParseDoc.Expr e [], b)) <$> alts'
          prd = ParseDoc.Expr prd' []
       in Right (doc [ParseDoc.Tag (ParseDoc.TagUnless prd conseq alts final)])

ifTags :: Spec
ifTags = do
  describe "Parse 'if' tags" $ do
    it "Basic 'if' tags" $ do
      let input =
            [r|{% if say_my_name %}
Heisenberg
{% else %}
Mr. White
{% endif %}|]
      runDocParse input
        `shouldBe` parsedIfTag
          (ParseDoc.ImmVar ["say_my_name"])
          [ParseDoc.Cont "\nHeisenberg\n"]
          []
          (Just [ParseDoc.Cont "\nMr. White\n"])

    it "Nested 'if' tags" $ do
      let input =
            [r|{% if blah %}
{% if blah.x %}
Foo
{% else %}
Bar
{% endif %}
{% else %}
Baz
{% endif %}|]
      runDocParse input
        `shouldBe` parsedIfTag
          (ParseDoc.ImmVar ["blah"])
          [ ParseDoc.Cont "\n"
          , ParseDoc.Tag
              ( ParseDoc.TagIf
                  (immVar ["blah", "x"])
                  [ParseDoc.Cont "\nFoo\n"]
                  []
                  (Just [ParseDoc.Cont "\nBar\n"])
              )
          , ParseDoc.Cont "\n"
          ]
          []
          (Just [ParseDoc.Cont "\nBaz\n"])
      let input2 =
            "{%if a%}{%if b%}{%if c%}Foo{%else%}Bar{%endif%}{%else%}Baz{%endif%}{%else%}Blah{%endif%}"
      runDocParse input2
        `shouldBe` parsedIfTag
          (ParseDoc.ImmVar ["a"])
          [ ParseDoc.Tag
              ( ParseDoc.TagIf
                  (immVar ["b"])
                  [ ParseDoc.Tag
                      ( ParseDoc.TagIf
                          (immVar ["c"])
                          [ParseDoc.Cont "Foo"]
                          []
                          (Just [ParseDoc.Cont "Bar"])
                      )
                  ]
                  []
                  (Just [ParseDoc.Cont "Baz"])
              )
          ]
          []
          (Just [ParseDoc.Cont "Blah"])

    it "'if' tag without alternative" $ do
      let input = "{% if 512 %}I love powers of two!{% endif %}"
      runDocParse input
        `shouldBe` parsedIfTag
          (ParseDoc.ImmNum 512)
          [ParseDoc.Cont "I love powers of two!"]
          []
          Nothing

    it "Strip away white space around 'if' tags" $ do
      let input =
            [r|
{%- if x %}
Blah
{%- else -%}
Not Blah
{% endif -%}
{%- if y %}
 Baz
{%- endif -%}
Foo
|]
      runDocParse input
        `shouldBe` Right
          ( doc
              [ ParseDoc.Tag
                  ( ParseDoc.TagIf
                      (immVar ["x"])
                      [ParseDoc.Cont "\nBlah"]
                      []
                      (Just [ParseDoc.Cont "Not Blah\n"])
                  )
              , ParseDoc.Tag
                  ( ParseDoc.TagIf
                      (immVar ["y"])
                      [ParseDoc.Cont "\n Baz"]
                      []
                      Nothing
                  )
              , ParseDoc.Cont "Foo\n"
              ]
          )

    it "'if' tags with 'elsif' and with 'else'" $
      do
        let input =
              [r|
{%- if x -%}
Foo
{%- elsif y -%}
Bar
{%- else -%}
Baz
{%- endif -%}
|]
        runDocParse input
        `shouldBe` parsedIfTag
          (ParseDoc.ImmVar ["x"])
          [ParseDoc.Cont "Foo"]
          [ (ParseDoc.ImmVar ["y"], [ParseDoc.Cont "Bar"])
          ]
          (Just [ParseDoc.Cont "Baz"])

    it "'if' tags with 'elsif' and without 'else'" $ do
      let input =
            [r|
{%- if one -%}
One
{%- elsif two -%}
Two
{%- elsif three -%}
Three
{%- elsif four -%}
Four
{%- endif -%}
|]
      runDocParse input
        `shouldBe` parsedIfTag
          (ParseDoc.ImmVar ["one"])
          [ParseDoc.Cont "One"]
          [ (ParseDoc.ImmVar ["two"], [ParseDoc.Cont "Two"])
          , (ParseDoc.ImmVar ["three"], [ParseDoc.Cont "Three"])
          , (ParseDoc.ImmVar ["four"], [ParseDoc.Cont "Four"])
          ]
          Nothing
  where
    parsedIfTag
      :: ParseDoc.BaseExpr
      -> [ParseDoc.Block]
      -> [(ParseDoc.BaseExpr, [ParseDoc.Block])]
      -> Maybe [ParseDoc.Block]
      -> Either String ParseDoc.Doc
    parsedIfTag prd' conseq alts' final =
      let alts = (\(e, b) -> (ParseDoc.Expr e [], b)) <$> alts'
          prd = ParseDoc.Expr prd' []
       in Right (doc [ParseDoc.Tag (ParseDoc.TagIf prd conseq alts final)])
    immVar :: ParseDoc.Var -> ParseDoc.Expr
    immVar v = ParseDoc.Expr (ParseDoc.ImmVar v) []
