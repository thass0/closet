module DocSpec (spec) where

import Test.Hspec
import Text.Megaparsec
import Text.RawString.QQ
import Data.Text
import Data.Either (isLeft)
import qualified Data.Yaml as Y
import Data.Yaml ((.=))
import qualified Doc
import qualified Doc


-- * Helpers

runTestParse :: Doc.Parser a -> Text -> Either String a
runTestParse parser input =
  case runParser parser "<in>" input of
    Right x -> Right x
    Left err -> Left (errorBundlePretty err)

runDocParse :: Text -> Either String Doc.Doc
runDocParse = runTestParse Doc.pDocument

docWithEmptyFM :: [Doc.Prog] -> Doc.Doc
docWithEmptyFM p = Doc.Doc
        { docFrontMatter = Y.object []
        , docProg = p
        }

-- * Specs

spec :: Spec
spec = do
    describe "Parse documents" $ do
        frontMatter
        statement

frontMatter :: Spec
frontMatter = do
    describe "Parse front matter" $ do
        it "simple front matter" $ do
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
            runDocParse input `shouldBe` (Right $ Doc.Doc
                { docFrontMatter = Y.object
                    [ "name" .= ("This is my name" :: Text)
                    , "age" .= (41 :: Int)
                    , "children" .= Y.array [ "Child1", "Child2", "Child3" ]
                    ]
                , docProg = [ Doc.LiteralContent "This is the content of this page!" ]
                })

        it "allow --- inside the front matter" $ do
            let input = [r|
---
title: "Blah --- my personal blog about Blah!"
---
Blah is the best filler word possible.|] 
            runDocParse input `shouldBe` (Right $ Doc.Doc
                { docFrontMatter = Y.object
                    [ "title" .= ("Blah --- my personal blog about Blah!" :: Text) ]
                , docProg = [ Doc.LiteralContent "Blah is the best filler word possible." ]
                })
        
        it  "closing --- must be at beginning of line" $ do
            let input1 = "---\r\ntitle: My blog ---\r\n" 
            runDocParse input1 `shouldSatisfy` isLeft
            let input2Win = "---\r\ntitle: My blog\r\n---\r\n"
            let input2Unix = "---\ntitle: My blog\n---\n"
            let input2Doc = Doc.Doc
                    { docFrontMatter = Y.object [ "title" .= ("My blog" :: Text) ]
                    , docProg = [ Doc.LiteralContent "" ]
                    }
            runDocParse input2Win `shouldBe` Right input2Doc
            runDocParse input2Unix `shouldBe` Right input2Doc
        
        it "commit to front matter after a single ---" $ do
            -- If the file contains a valid front matter start, the parser may not
            -- fall back to parsing this part of the file as normal file content.
            -- If the front matter that follows is invalid, the parser must fail entirely.
            let input = "---\ntitle: This blog has a front matter that does not end.\n"
            runDocParse input `shouldSatisfy` isLeft


-- ** Statement tests

statement :: Spec
statement = do
    describe "parse statements" $ do
        expressStatement

expressStatement :: Spec
expressStatement = do
    describe "Express statement" $ do
        it "simple express statement" $ do
            let parsedExpr e = Right (docWithEmptyFM [Doc.Stmt (Doc.StmtExpress e)])
            runDocParse "{{ blah.x }}" `shouldBe`
                    parsedExpr (Doc.Var ["blah", "x"])
            runDocParse "{{ blah.x.y.hello.world }}" `shouldBe`
                    parsedExpr (Doc.Var ["blah", "x", "y", "hello", "world"])
            runDocParse "{{ \"Hello, world!\" }}" `shouldBe`
                    parsedExpr (Doc.StringLiteral "Hello, world!")
            runDocParse "{{ 543 }}" `shouldBe`
                    parsedExpr (Doc.Number 543)
        it "invalid express statement"$ do
            runDocParse "{{ blah!x }}" `shouldSatisfy` isLeft
            runDocParse "{{ blah.x. }}" `shouldSatisfy` isLeft
            runDocParse "{{ blah * 91 }}" `shouldSatisfy` isLeft
            runDocParse "{{ 514blah }}" `shouldSatisfy` isLeft