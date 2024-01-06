module ParseDocSpec (spec) where

import Data.Either (isLeft)
import Data.Text (Text)
import Data.Yaml ((.=))
import qualified Data.Yaml as Y
import qualified ParseDoc
import Test.Hspec
import Text.Megaparsec
import Text.RawString.QQ (r)

-- * Helpers

runTestParse :: ParseDoc.Parser a -> Text -> Either String a
runTestParse parser input =
  case runParser parser "<in>" input of
    Right x -> Right x
    Left err -> Left (errorBundlePretty err)

runDocParse :: Text -> Either String ParseDoc.Doc
runDocParse = runTestParse ParseDoc.pDocument

doc :: [ParseDoc.Block] -> ParseDoc.Doc
doc p =
  ParseDoc.Doc
    { docFrontMatter = Y.object [],
      docBlocks = p
    }


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
                         { docFrontMatter =
                             Y.object
                               [ "name" .= ("This is my name" :: Text),
                                 "age" .= (41 :: Int),
                                 "children" .= Y.array ["Child1", "Child2", "Child3"]
                               ],
                           docBlocks = [ParseDoc.Cont "This is the content of this page!"]
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
                         { docFrontMatter =
                             Y.object
                               ["title" .= ("Blah --- my personal blog about Blah!" :: Text)],
                           docBlocks = [ParseDoc.Cont "Blah is the best filler word possible."]
                         }
                   )

    it "Closing \"---\" must be at beginning of line" $ do
      let input1 = "---\r\ntitle: My blog ---\r\n"
      runDocParse input1 `shouldSatisfy` isLeft
      let input2Win = "---\r\ntitle: My blog\r\n---\r\n"
      let input2Unix = "---\ntitle: My blog\n---\n"
      let input2Doc =
            ParseDoc.Doc
              { docFrontMatter = Y.object ["title" .= ("My blog" :: Text)],
                docBlocks = [ParseDoc.Cont ""]
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
        `shouldBe`
          parsedCondition
            (ParseDoc.Expr
              (ParseDoc.ExprAnd
                (ParseDoc.ImmVar ["a"])
                (ParseDoc.ExprOr (ParseDoc.ImmVar ["b"]) (ParseDoc.ImmVar ["c"]))
              )
              []
            )
      parseCondition "a or b and c or d"
        `shouldBe`
          parsedCondition
            (ParseDoc.Expr
              (ParseDoc.ExprOr
                (ParseDoc.ImmVar ["a"])
                (ParseDoc.ExprAnd
                  (ParseDoc.ImmVar ["b"])
                  (ParseDoc.ExprOr
                    (ParseDoc.ImmVar ["c"])
                    (ParseDoc.ImmVar ["d"])
                  )
                )
              )
              []
            )

    it "'and'" $ parseCondition "a and b"
      `shouldBe`
        parsedCondition
          ( ParseDoc.Expr
            ( ParseDoc.ExprAnd (ParseDoc.ImmVar ["a"]) (ParseDoc.ImmVar ["b"])) [])

    it "'or'" $ parseCondition "a or b"
      `shouldBe`
        parsedCondition
          ( ParseDoc.Expr
            ( ParseDoc.ExprOr (ParseDoc.ImmVar ["a"]) (ParseDoc.ImmVar ["b"])) [])

    it "'=='" $ parseCondition "a == b"
      `shouldBe`
        parsedCondition
          ( ParseDoc.Expr
            ( ParseDoc.ExprEq (ParseDoc.ImmVar ["a"]) (ParseDoc.ImmVar ["b"])) [])

    it "'!='" $ parseCondition "a != b"
      `shouldBe`
        parsedCondition
          ( ParseDoc.Expr
            ( ParseDoc.ExprNeq (ParseDoc.ImmVar ["a"]) (ParseDoc.ImmVar ["b"])) [])

    it "'>'" $ parseCondition "a > b"
      `shouldBe`
        parsedCondition
          ( ParseDoc.Expr
            ( ParseDoc.ExprGt (ParseDoc.ImmVar ["a"]) (ParseDoc.ImmVar ["b"])) [])

    it "'<'" $ parseCondition "a < b"
      `shouldBe`
        parsedCondition
          ( ParseDoc.Expr
            ( ParseDoc.ExprLt (ParseDoc.ImmVar ["a"]) (ParseDoc.ImmVar ["b"])) [])

    it "'>='" $ parseCondition "a >= b"
      `shouldBe`
        parsedCondition
          ( ParseDoc.Expr
            ( ParseDoc.ExprGeq (ParseDoc.ImmVar ["a"]) (ParseDoc.ImmVar ["b"])) [])

    it "'<='" $ parseCondition "a <= b"
      `shouldBe`
        parsedCondition
          ( ParseDoc.Expr
            ( ParseDoc.ExprLeq (ParseDoc.ImmVar ["a"]) (ParseDoc.ImmVar ["b"])) [])
  where
    parseCondition con =
      runDocParse $ "{% if " <> con <> " %}Blah{% endif %}"
    parsedCondition e = Right $ doc
      [ParseDoc.Tag
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
    describe "Numeric filters" $ do
      it "'plus'" $
        "plus: 4891"
          `filterShouldBe` ParseDoc.FilterPlus (ParseDoc.NumberyNum 4891)
      it "'minus'" $
        "minus: 51"
          `filterShouldBe` ParseDoc.FilterMinus (ParseDoc.NumberyNum 51)
      it "'times'" $
        "times: -642"
          `filterShouldBe` ParseDoc.FilterTimes (ParseDoc.NumberyNum (-642))
      it "'divided_by'" $
        "divided_by: y"
          `filterShouldBe` ParseDoc.FilterDividedBy (ParseDoc.NumberyVar ["y"])
      it "'modulo'" $
        "modulo: site.x"
          `filterShouldBe` ParseDoc.FilterModulo (ParseDoc.NumberyVar ["site", "x"])
      it "'at_least'" $ do
        "at_least: 19"
          `filterShouldBe` ParseDoc.FilterAtLeast (ParseDoc.NumberyNum 19)
        runDocParse "{{ 19 | at_least: \"Blah\" }}" `shouldSatisfy` isLeft
      it "'at_most'" $ do
        "at_most: 19"
          `filterShouldBe` ParseDoc.FilterAtMost (ParseDoc.NumberyNum 19)
        runDocParse "{{ 19 | at_most: \"Blah\" }}" `shouldSatisfy` isLeft
      it "'abs'" $ do
        "abs" `filterShouldBe` ParseDoc.FilterAbs
        runDocParse "{{ -32 | abs | abs|abs|abs  | abs}}"
          `shouldBe` parsedExpr (ParseDoc.ImmNum (-32)) (replicate 5 ParseDoc.FilterAbs)
      it "'ceil'" $ "ceil" `filterShouldBe` ParseDoc.FilterCeil
      it "'floor'" $ "floor" `filterShouldBe` ParseDoc.FilterFloor
      it "'round'" $ "round" `filterShouldBe` ParseDoc.FilterRound

    describe "Array and object filters" $ do
      it "'append'" $ do
        "append: \"World!\""
          `filterShouldBe` ParseDoc.FilterAppend (ParseDoc.StringyLit "World!")
        runDocParse "{{ \"Blah\" | append: 91 }}" `shouldSatisfy` isLeft
      it "'concat'" $ "concat: wowow" `filterShouldBe` ParseDoc.FilterConcat ["wowow"]
      it "'first'" $ "first" `filterShouldBe` ParseDoc.FilterFirst
      it "'last'" $ "last" `filterShouldBe` ParseDoc.FilterLast
      it "'join'" $
        "join: blah"
          `filterShouldBe` ParseDoc.FilterJoin (ParseDoc.StringyVar ["blah"])
      it "'reverse'" $ "reverse" `filterShouldBe` ParseDoc.FilterReverse
      it "'sort'" $ "sort" `filterShouldBe` ParseDoc.FilterSort
      it "'sort_natural'" $ "sort_natural" `filterShouldBe` ParseDoc.FilterSortNatural
      it "'map'" $
        "map: \"blah\""
          `filterShouldBe` ParseDoc.FilterMap (ParseDoc.StringyLit "blah")
      it "'compact'" $ "compact" `filterShouldBe` ParseDoc.FilterCompact
      it "'sum'" $ do
        "sum" `filterShouldBe` ParseDoc.FilterSum Nothing
        "sum: \"category\""
          `filterShouldBe` ParseDoc.FilterSum (Just (ParseDoc.StringyLit "category"))
      it "'uniq'" $ "uniq" `filterShouldBe` ParseDoc.FilterUniq
      it "'where'" $ do
        "where: \"x\""
          `filterShouldBe` ParseDoc.FilterWhere (ParseDoc.StringyLit "x") Nothing
        "where: blah, foo"
          `filterShouldBe` ParseDoc.FilterWhere
            (ParseDoc.StringyVar ["blah"])
            (Just (ParseDoc.StringyVar ["foo"]))

    describe "String filters" $ do
      it "'capitalize'" $ "capitalize" `filterShouldBe` ParseDoc.FilterCapitalize
      it "'upcase'" $ "upcase" `filterShouldBe` ParseDoc.FilterUpcase
      it "'downcase'" $ "downcase" `filterShouldBe` ParseDoc.FilterDowncase
      it "'lstrip'" $ "lstrip" `filterShouldBe` ParseDoc.FilterLStrip
      it "'rstrip'" $ "rstrip" `filterShouldBe` ParseDoc.FilterRStrip
      it "'prepend'" $
        "prepend: \"blah\""
          `filterShouldBe` ParseDoc.FilterPrepend (ParseDoc.StringyLit "blah")
      it "'replace'" $
        "replace: the.string, \"blah\""
          `filterShouldBe` ParseDoc.FilterReplace
            (ParseDoc.StringyVar ["the", "string"])
            (ParseDoc.StringyLit "blah")
      it "'replace_first'" $
        "replace_first: \"blah\" , page.title "
          `filterShouldBe` ParseDoc.FilterReplaceFirst
            (ParseDoc.StringyLit "blah")
            (ParseDoc.StringyVar ["page", "title"])
      it "'remove'" $
        "remove: the.string"
          `filterShouldBe` ParseDoc.FilterRemove (ParseDoc.StringyVar ["the", "string"])
      it "'remove_first'" $
        "remove_first: \"blah\""
          `filterShouldBe` ParseDoc.FilterRemoveFirst (ParseDoc.StringyLit "blah")
      it "'size'" $ "size" `filterShouldBe` ParseDoc.FilterSize
      it "'slice'" $ do
        "slice: 4"
          `filterShouldBe` ParseDoc.FilterSlice (ParseDoc.NumberyNum 4) Nothing
        "slice: 5, 9"
          `filterShouldBe` ParseDoc.FilterSlice (ParseDoc.NumberyNum 5) (Just (ParseDoc.NumberyNum 9))
      it "'split'" $
        "split: \",\""
          `filterShouldBe` ParseDoc.FilterSplit (ParseDoc.StringyLit ",")
      it "'strip'" $ "strip" `filterShouldBe` ParseDoc.FilterStrip
      it "'strip_html'" $ "strip_html" `filterShouldBe` ParseDoc.FilterStripHtml
      it "'strip_newlines'" $ "strip_newlines" `filterShouldBe` ParseDoc.FilterStripNewlines
      it "'escape'" $ "escape" `filterShouldBe` ParseDoc.FilterEscape
      it "'escape_once'" $ "escape_once" `filterShouldBe` ParseDoc.FilterEscapeOnce
      it "'newline_to_br'" $ "newline_to_br" `filterShouldBe` ParseDoc.FilterNewlineToBr
      it "'truncate'" $ do
        "truncate: 42"
          `filterShouldBe` ParseDoc.FilterTruncate (ParseDoc.NumberyNum 42) Nothing
        "truncate: 64 ,\"blah\""
          `filterShouldBe` ParseDoc.FilterTruncate
            (ParseDoc.NumberyNum 64)
            (Just (ParseDoc.StringyLit "blah"))
      it "'truncatewords'" $ do
        "truncatewords: 259"
          `filterShouldBe` ParseDoc.FilterTruncateWords (ParseDoc.NumberyNum 259) Nothing
        "truncatewords:49,my.ellipse"
          `filterShouldBe` ParseDoc.FilterTruncateWords
            (ParseDoc.NumberyNum 49)
            (Just (ParseDoc.StringyVar ["my", "ellipse"]))
      it "'date'" $
        "date: \"%a, %b %d, %y\""
          `filterShouldBe` ParseDoc.FilterDate (ParseDoc.StringyLit "%a, %b %d, %y")
      it "'url_decode'" $ "url_decode" `filterShouldBe` ParseDoc.FilterUrlDecode
      it "'url_encode'" $ "url_encode" `filterShouldBe` ParseDoc.FilterUrlEncode

    describe "Miscellaneous filters" $ do
      it "'default'" $
        "default: site.title"
          `filterShouldBe` ParseDoc.FilterDefault (ParseDoc.ImmVar ["site", "title"])
  where
    parsedExpr
      :: ParseDoc.BaseExpr
      -> [ParseDoc.FilterExpr]
      -> Either String ParseDoc.Doc
    parsedExpr imm f = Right
      (doc
        [ParseDoc.Tag
          (ParseDoc.TagExpress (ParseDoc.Expr imm f))])
    filterShouldBe filterString fil =
      runDocParse ("{{ x | " <> filterString <> " }}")
        `shouldBe` parsedExpr (ParseDoc.ImmVar ["x"]) [fil]


-- ** Tests on parsing tags

tags :: Spec
tags = do
  describe "Parse tags" $ do
    expressTags
    ifTags

expressTags :: Spec
expressTags = do
  describe "Express tags" $ do
    it "Basic express tags" $ do
      runDocParse "{{ blah.x }}"
        `shouldBe` parsedImmExpr (ParseDoc.ImmVar ["blah", "x"])
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
              [ ParseDoc.Cont " foo",
                ParseDoc.Tag (ParseDoc.TagExpress (ParseDoc.Expr (ParseDoc.ImmVar ["blah"]) [])),
                ParseDoc.Cont "bar "
              ]
          )
  where
    parsedImmExpr imm = Right
          (doc [ParseDoc.Tag (ParseDoc.TagExpress (ParseDoc.Expr imm []))])

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
          [ ParseDoc.Cont "\n",
            ParseDoc.Tag
              ( ParseDoc.TagIf
                  (ParseDoc.immVar ["blah", "x"])
                  [ParseDoc.Cont "\nFoo\n"]
                  []
                  (Just [ParseDoc.Cont "\nBar\n"])
              ),
            ParseDoc.Cont "\n"
          ]
          []
          (Just [ParseDoc.Cont "\nBaz\n"])
      let input2 = "{%if a%}{%if b%}{%if c%}Foo{%else%}Bar{%endif%}{%else%}Baz{%endif%}{%else%}Blah{%endif%}"
      runDocParse input2
        `shouldBe` parsedIfTag
          (ParseDoc.ImmVar ["a"])
          [ ParseDoc.Tag
              ( ParseDoc.TagIf
                  (ParseDoc.immVar ["b"])
                  [ ParseDoc.Tag
                      ( ParseDoc.TagIf
                          (ParseDoc.immVar ["c"])
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
                      (ParseDoc.immVar ["x"])
                      [ParseDoc.Cont "\nBlah"]
                      []
                      (Just [ParseDoc.Cont "Not Blah\n"])
                  ),
                ParseDoc.Tag
                  ( ParseDoc.TagIf
                      (ParseDoc.immVar ["y"])
                      [ParseDoc.Cont "\n Baz"]
                      []
                      Nothing
                  ),
                ParseDoc.Cont "Foo\n"
              ]
          )

    it "'if' tags with 'elsif' and with 'else'" $ do
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
          , (ParseDoc.ImmVar ["four"], [ParseDoc.Cont "Four"]) ]
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
