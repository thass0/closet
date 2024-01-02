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

docWithEmptyFM :: [ParseDoc.Block] -> ParseDoc.Doc
docWithEmptyFM p =
  ParseDoc.Doc
    { docFrontMatter = Y.object [],
      docBlocks = p
    }

parsedImmExpr :: ParseDoc.ImmExpr -> Either String ParseDoc.Doc
parsedImmExpr imm =
  Right
    (docWithEmptyFM [ParseDoc.Stmt (ParseDoc.StmtExpress (ParseDoc.Expr imm []))])

parsedExpr :: ParseDoc.ImmExpr -> [ParseDoc.FilterExpr] -> Either String ParseDoc.Doc
parsedExpr imm f =
  Right
    (docWithEmptyFM [ParseDoc.Stmt (ParseDoc.StmtExpress (ParseDoc.Expr imm f))])

parsedIfStmt ::
  ParseDoc.ImmExpr ->
  [ParseDoc.Block] ->
  Maybe [ParseDoc.Block] ->
  Either String ParseDoc.Doc
parsedIfStmt predicate consequent alternative =
  Right
    (docWithEmptyFM [ParseDoc.Stmt (ParseDoc.StmtIf predicate consequent alternative)])

-- * Specs

spec :: Spec
spec = do
  describe "Parse documents" $ do
    frontMatter
    statement
    it "white space at start of input" $ do
      runDocParse "   This is my entire document! " `shouldBe` Right (docWithEmptyFM [ParseDoc.LiteralContent "   This is my entire document! "])

frontMatter :: Spec
frontMatter = do
  describe "Parse front matter" $ do
    it "Simple front matter" $ do
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
                           docBlocks = [ParseDoc.LiteralContent "This is the content of this page!"]
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
                           docBlocks = [ParseDoc.LiteralContent "Blah is the best filler word possible."]
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
                docBlocks = [ParseDoc.LiteralContent ""]
              }
      runDocParse input2Win `shouldBe` Right input2Doc
      runDocParse input2Unix `shouldBe` Right input2Doc

    it "Commit to front matter after a single \"---\"" $ do
      -- If the file contains a valid front matter start, the parser may not
      -- fall back to parsing this part of the file as normal file content.
      -- If the front matter that follows is invalid, the parser must fail entirely.
      let input = "---\ntitle: This blog has a front matter that does not end.\n"
      runDocParse input `shouldSatisfy` isLeft

-- ** Statement tests

statement :: Spec
statement = do
  describe "Parse statements" $ do
    expressStatement
    filteredStatement
    ifStatement

expressStatement :: Spec
expressStatement = do
  describe "Express statement" $ do
    it "Simple express statement" $ do
      runDocParse "{{ blah.x }}"
        `shouldBe` parsedImmExpr (ParseDoc.ImmVar ["blah", "x"])
      runDocParse "{{ blah.x.y.hello.world }}"
        `shouldBe` parsedImmExpr (ParseDoc.ImmVar ["blah", "x", "y", "hello", "world"])
      runDocParse "{{ \"Hello, world!\" }}"
        `shouldBe` parsedImmExpr (ParseDoc.ImmStringLiteral "Hello, world!")
      runDocParse "{{ 543 }}"
        `shouldBe` parsedImmExpr (ParseDoc.ImmNumber 543)
      runDocParse "{{ -61 }}"
        `shouldBe` parsedImmExpr (ParseDoc.ImmNumber (-61))
      runDocParse "{{blah}}"
        `shouldBe` parsedImmExpr (ParseDoc.ImmVar ["blah"])
    it "Invalid express statement" $ do
      runDocParse "{{ blah!x }}" `shouldSatisfy` isLeft
      runDocParse "{{ blah.x. }}" `shouldSatisfy` isLeft
      runDocParse "{{ blah * 91 }}" `shouldSatisfy` isLeft
      runDocParse "{{ 514blah }}" `shouldSatisfy` isLeft
    it "strip whitespace in express statements" $ do
      runDocParse " foo {{- blah -}} bar "
        `shouldBe` Right
          ( docWithEmptyFM
              [ ParseDoc.LiteralContent " foo",
                ParseDoc.Stmt (ParseDoc.StmtExpress (ParseDoc.Expr (ParseDoc.ImmVar ["blah"]) [])),
                ParseDoc.LiteralContent "bar "
              ]
          )

filteredStatement :: Spec
filteredStatement = do
  describe "Filtered statement" $ do
    let filterShouldBe filterString fil =
          runDocParse ("{{ x | " <> filterString <> " }}")
            `shouldBe` parsedExpr (ParseDoc.ImmVar ["x"]) [fil]
    describe "Numeric filters" $ do
      it "'plus'" $
        "plus: 4891"
          `filterShouldBe` ParseDoc.FilterPlus (ParseDoc.NImmNumber 4891)
      it "'minus'" $
        "minus: 51"
          `filterShouldBe` ParseDoc.FilterMinus (ParseDoc.NImmNumber 51)
      it "'times'" $
        "times: -642"
          `filterShouldBe` ParseDoc.FilterTimes (ParseDoc.NImmNumber (-642))
      it "'divided_by'" $
        "divided_by: y"
          `filterShouldBe` ParseDoc.FilterDividedBy (ParseDoc.NImmVar ["y"])
      it "'modulo'" $
        "modulo: site.x"
          `filterShouldBe` ParseDoc.FilterModulo (ParseDoc.NImmVar ["site", "x"])
      it "'at_least'" $ do
        "at_least: 19"
          `filterShouldBe` ParseDoc.FilterAtLeast (ParseDoc.NImmNumber 19)
        runDocParse "{{ 19 | at_least: \"Blah\" }}" `shouldSatisfy` isLeft
      it "'at_most'" $ do
        "at_most: 19"
          `filterShouldBe` ParseDoc.FilterAtMost (ParseDoc.NImmNumber 19)
        runDocParse "{{ 19 | at_most: \"Blah\" }}" `shouldSatisfy` isLeft
      it "'abs'" $ do
        "abs" `filterShouldBe` ParseDoc.FilterAbs
        runDocParse "{{ -32 | abs | abs|abs|abs  | abs}}"
          `shouldBe` parsedExpr (ParseDoc.ImmNumber (-32)) (replicate 5 ParseDoc.FilterAbs)
      it "'ceil'" $ "ceil" `filterShouldBe` ParseDoc.FilterCeil
      it "'floor'" $ "floor" `filterShouldBe` ParseDoc.FilterFloor
      it "'round'" $ "round" `filterShouldBe` ParseDoc.FilterRound

    describe "Array and object filters" $ do
      it "'append'" $ do
        "append: \"World!\""
          `filterShouldBe` ParseDoc.FilterAppend (ParseDoc.SImmStringLiteral "World!")
        runDocParse "{{ \"Blah\" | append: 91 }}" `shouldSatisfy` isLeft
      it "'concat'" $ "concat: wowow" `filterShouldBe` ParseDoc.FilterConcat ["wowow"]
      it "'first'" $ "first" `filterShouldBe` ParseDoc.FilterFirst
      it "'last'" $ "last" `filterShouldBe` ParseDoc.FilterLast
      it "'join'" $
        "join: blah"
          `filterShouldBe` ParseDoc.FilterJoin (ParseDoc.SImmVar ["blah"])
      it "'reverse'" $ "reverse" `filterShouldBe` ParseDoc.FilterReverse
      it "'sort'" $ "sort" `filterShouldBe` ParseDoc.FilterSort
      it "'sort_natural'" $ "sort_natural" `filterShouldBe` ParseDoc.FilterSortNatural
      it "'map'" $
        "map: \"blah\""
          `filterShouldBe` ParseDoc.FilterMap (ParseDoc.SImmStringLiteral "blah")
      it "'compact'" $ "compact" `filterShouldBe` ParseDoc.FilterCompact
      it "'sum'" $ do
        "sum" `filterShouldBe` ParseDoc.FilterSum Nothing
        "sum: \"category\""
          `filterShouldBe` ParseDoc.FilterSum (Just (ParseDoc.SImmStringLiteral "category"))
      it "'uniq'" $ "uniq" `filterShouldBe` ParseDoc.FilterUniq
      it "'where'" $ do
        "where: \"x\""
          `filterShouldBe` ParseDoc.FilterWhere (ParseDoc.SImmStringLiteral "x") Nothing
        "where: blah, foo"
          `filterShouldBe` ParseDoc.FilterWhere
            (ParseDoc.SImmVar ["blah"])
            (Just (ParseDoc.SImmVar ["foo"]))

    describe "String filters" $ do
      it "'capitalize'" $ "capitalize" `filterShouldBe` ParseDoc.FilterCapitalize
      it "'upcase'" $ "upcase" `filterShouldBe` ParseDoc.FilterUpcase
      it "'downcase'" $ "downcase" `filterShouldBe` ParseDoc.FilterDowncase
      it "'lstrip'" $ "lstrip" `filterShouldBe` ParseDoc.FilterLStrip
      it "'rstrip'" $ "rstrip" `filterShouldBe` ParseDoc.FilterRStrip
      it "'prepend'" $
        "prepend: \"blah\""
          `filterShouldBe` ParseDoc.FilterPrepend (ParseDoc.SImmStringLiteral "blah")
      it "'replace'" $
        "replace: the.string, \"blah\""
          `filterShouldBe` ParseDoc.FilterReplace
            (ParseDoc.SImmVar ["the", "string"])
            (ParseDoc.SImmStringLiteral "blah")
      it "'replace_first'" $
        "replace_first: \"blah\" , page.title "
          `filterShouldBe` ParseDoc.FilterReplaceFirst
            (ParseDoc.SImmStringLiteral "blah")
            (ParseDoc.SImmVar ["page", "title"])
      it "'remove'" $
        "remove: the.string"
          `filterShouldBe` ParseDoc.FilterRemove (ParseDoc.SImmVar ["the", "string"])
      it "'remove_first'" $
        "remove_first: \"blah\""
          `filterShouldBe` ParseDoc.FilterRemoveFirst (ParseDoc.SImmStringLiteral "blah")
      it "'size'" $ "size" `filterShouldBe` ParseDoc.FilterSize
      it "'slice'" $ do
        "slice: 4"
          `filterShouldBe` ParseDoc.FilterSlice (ParseDoc.NImmNumber 4) Nothing
        "slice: 5, 9"
          `filterShouldBe` ParseDoc.FilterSlice (ParseDoc.NImmNumber 5) (Just (ParseDoc.NImmNumber 9))
      it "'split'" $
        "split: \",\""
          `filterShouldBe` ParseDoc.FilterSplit (ParseDoc.SImmStringLiteral ",")
      it "'strip'" $ "strip" `filterShouldBe` ParseDoc.FilterStrip
      it "'strip_html'" $ "strip_html" `filterShouldBe` ParseDoc.FilterStripHtml
      it "'strip_newlines'" $ "strip_newlines" `filterShouldBe` ParseDoc.FilterStripNewlines
      it "'escape'" $ "escape" `filterShouldBe` ParseDoc.FilterEscape
      it "'escape_once'" $ "escape_once" `filterShouldBe` ParseDoc.FilterEscapeOnce
      it "'newline_to_br'" $ "newline_to_br" `filterShouldBe` ParseDoc.FilterNewlineToBr
      it "'truncate'" $ do
        "truncate: 42"
          `filterShouldBe` ParseDoc.FilterTruncate (ParseDoc.NImmNumber 42) Nothing
        "truncate: 64 ,\"blah\""
          `filterShouldBe` ParseDoc.FilterTruncate
            (ParseDoc.NImmNumber 64)
            (Just (ParseDoc.SImmStringLiteral "blah"))
      it "'truncatewords'" $ do
        "truncatewords: 259"
          `filterShouldBe` ParseDoc.FilterTruncateWords (ParseDoc.NImmNumber 259) Nothing
        "truncatewords:49,my.ellipse"
          `filterShouldBe` ParseDoc.FilterTruncateWords
            (ParseDoc.NImmNumber 49)
            (Just (ParseDoc.SImmVar ["my", "ellipse"]))
      it "'date'" $
        "date: \"%a, %b %d, %y\""
          `filterShouldBe` ParseDoc.FilterDate (ParseDoc.SImmStringLiteral "%a, %b %d, %y")
      it "'url_decode'" $ "url_decode" `filterShouldBe` ParseDoc.FilterUrlDecode
      it "'url_encode'" $ "url_encode" `filterShouldBe` ParseDoc.FilterUrlEncode

    describe "Miscellaneous filters" $ do
      it "'default'" $
        "default: site.title"
          `filterShouldBe` ParseDoc.FilterDefault (ParseDoc.ImmVar ["site", "title"])

ifStatement :: Spec
ifStatement = do
  describe "if statement" $ do
    it "simple if statement" $ do
      let input =
            [r|{% if say_my_name %}
Heisenberg
{% else %}
Mr. White
{% endif %}|]
      runDocParse input
        `shouldBe` parsedIfStmt
          (ParseDoc.ImmVar ["say_my_name"])
          [ParseDoc.LiteralContent "\nHeisenberg\n"]
          (Just [ParseDoc.LiteralContent "\nMr. White\n"])
    it "nested if statements" $ do
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
        `shouldBe` parsedIfStmt
          (ParseDoc.ImmVar ["blah"])
          [ ParseDoc.LiteralContent "\n",
            ParseDoc.Stmt
              ( ParseDoc.StmtIf
                  (ParseDoc.ImmVar ["blah", "x"])
                  [ParseDoc.LiteralContent "\nFoo\n"]
                  (Just [ParseDoc.LiteralContent "\nBar\n"])
              ),
            ParseDoc.LiteralContent "\n"
          ]
          (Just [ParseDoc.LiteralContent "\nBaz\n"])
      let input2 = "{%if a%}{%if b%}{%if c%}Foo{%else%}Bar{%endif%}{%else%}Baz{%endif%}{%else%}Blah{%endif%}"
      runDocParse input2
        `shouldBe` parsedIfStmt
          (ParseDoc.ImmVar ["a"])
          [ ParseDoc.Stmt
              ( ParseDoc.StmtIf
                  (ParseDoc.ImmVar ["b"])
                  [ ParseDoc.Stmt
                      ( ParseDoc.StmtIf
                          (ParseDoc.ImmVar ["c"])
                          [ParseDoc.LiteralContent "Foo"]
                          (Just [ParseDoc.LiteralContent "Bar"])
                      )
                  ]
                  (Just [ParseDoc.LiteralContent "Baz"])
              )
          ]
          (Just [ParseDoc.LiteralContent "Blah"])
    it "if statement without alternative" $ do
      let input = "{% if 512 %}I love powers of two!{% endif %}"
      runDocParse input
        `shouldBe` parsedIfStmt
          (ParseDoc.ImmNumber 512)
          [ParseDoc.LiteralContent "I love powers of two!"]
          Nothing
    it "white space stripped in if statement" $ do
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
Foo|]
      runDocParse input
        `shouldBe` Right
          ( docWithEmptyFM
              [ ParseDoc.LiteralContent "",
                ParseDoc.Stmt
                  ( ParseDoc.StmtIf
                      (ParseDoc.ImmVar ["x"])
                      [ParseDoc.LiteralContent "\nBlah"]
                      (Just [ParseDoc.LiteralContent "Not Blah\n"])
                  ),
                ParseDoc.LiteralContent "",
                ParseDoc.Stmt
                  ( ParseDoc.StmtIf
                      (ParseDoc.ImmVar ["y"])
                      [ParseDoc.LiteralContent "\n Baz"]
                      Nothing
                  ),
                ParseDoc.LiteralContent "Foo"
              ]
          )