module ParseDocSpec (spec) where

import Test.Hspec
import Text.Megaparsec
import Text.RawString.QQ
import Data.Text hiding (replicate)
import Data.Either (isLeft)
import qualified Data.Yaml as Y
import Data.Yaml ((.=))
import qualified ParseDoc


-- * Helpers

runTestParse :: ParseDoc.Parser a -> Text -> Either String a
runTestParse parser input =
  case runParser parser "<in>" input of
    Right x -> Right x
    Left err -> Left (errorBundlePretty err)

runDocParse :: Text -> Either String ParseDoc.Doc
runDocParse = runTestParse ParseDoc.pDocument

docWithEmptyFM :: [ParseDoc.Prog] -> ParseDoc.Doc
docWithEmptyFM p = ParseDoc.Doc
        { docFrontMatter = Y.object []
        , docProg = p
        }

parsedImmExpr :: ParseDoc.ImmExpr -> Either String ParseDoc.Doc
parsedImmExpr imm = Right
        (docWithEmptyFM [ParseDoc.Stmt (ParseDoc.StmtExpress (ParseDoc.Expr imm []))])

parsedExpr :: ParseDoc.ImmExpr -> [ParseDoc.FilterExpr] -> Either String ParseDoc.Doc
parsedExpr imm f = Right
        (docWithEmptyFM [ParseDoc.Stmt (ParseDoc.StmtExpress (ParseDoc.Expr imm f))])


-- * Specs

spec :: Spec
spec = do
    describe "Parse documents" $ do
        frontMatter
        statement

frontMatter :: Spec
frontMatter = do
    describe "Parse front matter" $ do
        it "Simple front matter" $ do
            let input = [r|
---
name: "This is my name"
age: 41
children:
    - Child1
    - Child2
    - Child3
---
This is the content of this page!|]
            runDocParse input `shouldBe` (Right $ ParseDoc.Doc
                { docFrontMatter = Y.object
                    [ "name" .= ("This is my name" :: Text)
                    , "age" .= (41 :: Int)
                    , "children" .= Y.array [ "Child1", "Child2", "Child3" ]
                    ]
                , docProg = [ ParseDoc.LiteralContent "This is the content of this page!" ]
                })

        it "Allow \"---\" inside the front matter" $ do
            let input = [r|
---
title: "Blah --- my personal blog about Blah!"
---
Blah is the best filler word possible.|] 
            runDocParse input `shouldBe` (Right $ ParseDoc.Doc
                { docFrontMatter = Y.object
                    [ "title" .= ("Blah --- my personal blog about Blah!" :: Text) ]
                , docProg = [ ParseDoc.LiteralContent "Blah is the best filler word possible." ]
                })
        
        it  "Closing \"---\" must be at beginning of line" $ do
            let input1 = "---\r\ntitle: My blog ---\r\n" 
            runDocParse input1 `shouldSatisfy` isLeft
            let input2Win = "---\r\ntitle: My blog\r\n---\r\n"
            let input2Unix = "---\ntitle: My blog\n---\n"
            let input2Doc = ParseDoc.Doc
                    { docFrontMatter = Y.object [ "title" .= ("My blog" :: Text) ]
                    , docProg = [ ParseDoc.LiteralContent "" ]
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

expressStatement :: Spec
expressStatement = do
    describe "Express statement" $ do
        it "Simple express statement" $ do
            runDocParse "{{ blah.x }}" `shouldBe`
                    parsedImmExpr (ParseDoc.Var ["blah", "x"])
            runDocParse "{{ blah.x.y.hello.world }}" `shouldBe`
                    parsedImmExpr (ParseDoc.Var ["blah", "x", "y", "hello", "world"])
            runDocParse "{{ \"Hello, world!\" }}" `shouldBe`
                    parsedImmExpr (ParseDoc.StringLiteral "Hello, world!")
            runDocParse "{{ 543 }}" `shouldBe`
                    parsedImmExpr (ParseDoc.Number 543)
            runDocParse "{{ -61 }}" `shouldBe`
                    parsedImmExpr (ParseDoc.Number (-61))
            runDocParse "{{blah}}" `shouldBe`
                    parsedImmExpr (ParseDoc.Var ["blah"])
        it "Invalid express statement"$ do
            runDocParse "{{ blah!x }}" `shouldSatisfy` isLeft
            runDocParse "{{ blah.x. }}" `shouldSatisfy` isLeft
            runDocParse "{{ blah * 91 }}" `shouldSatisfy` isLeft
            runDocParse "{{ 514blah }}" `shouldSatisfy` isLeft

filteredStatement :: Spec
filteredStatement = do
    describe "Filtered statement" $ do
        it "'abs' filter" $ do
            runDocParse "{{ -32 | abs }}" `shouldBe`
                    parsedExpr (ParseDoc.Number (-32)) [ParseDoc.FilterAbs]
            runDocParse "{{ -32 | abs | abs|abs|abs  | abs}}" `shouldBe`
                    parsedExpr (ParseDoc.Number (-32)) (replicate 5 ParseDoc.FilterAbs)
        it "'append' filter" $ do
            runDocParse "{{ \"Hello, \" | append: \"World!\" }}" `shouldBe`
                    parsedExpr (ParseDoc.StringLiteral "Hello, ")
                               [ParseDoc.FilterAppend (ParseDoc.StringLiteral "World!")]
            runDocParse "{{ \"Blah\" | append: 91 }}" `shouldSatisfy` isLeft
        it "'at_least' filter" $ do
            runDocParse "{{ 5 | at_least: 19 }}" `shouldBe`
                    parsedExpr (ParseDoc.Number 5) [ParseDoc.FilterAtLeast (ParseDoc.Number 19)]
            runDocParse "{{ 19 | at_least: \"Blah\" }}" `shouldSatisfy` isLeft