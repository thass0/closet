module DocSpec (spec) where

import Test.Hspec
import Text.Megaparsec
import Text.RawString.QQ
import Data.Text
import Data.Either (isLeft)
import qualified Data.Yaml as Y
import Data.Yaml ((.=))
import Doc

spec :: Spec
spec = do
    describe "Parse documents" $ do
        frontMatter

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
            (runTestParse pDocument input) `shouldBe` (Right $ Doc
                { docFrontMatter = Y.object
                    [ "name" .= ("This is my name" :: Text)
                    , "age" .= (41 :: Int)
                    , "children" .= Y.array [ "Child1", "Child2", "Child3" ]
                    ]
                , docProg = [ DocLiteral "This is the content of this page!" ]
                })

        it "allow --- inside the front matter" $ do
            let input = [r|
---
title: "Blah --- my personal blog about Blah!"
---
Blah is the best filler word possible.|] 
            (runTestParse pDocument input) `shouldBe` (Right $ Doc
                { docFrontMatter = Y.object
                    [ "title" .= ("Blah --- my personal blog about Blah!" :: Text) ]
                , docProg = [ DocLiteral "Blah is the best filler word possible." ]
                })
        
        it  "closing --- must be at beginning of line" $ do
            let input1 = "---\r\ntitle: My blog ---\r\n" 
            (runTestParse pDocument input1) `shouldSatisfy` isLeft
            let input2Win = "---\r\ntitle: My blog\r\n---\r\n"
            let input2Unix = "---\ntitle: My blog\n---\n"
            let input2Doc = Doc
                    { docFrontMatter = Y.object [ "title" .= ("My blog" :: Text) ]
                    , docProg = [ DocLiteral "" ]
                    }
            (runTestParse pDocument input2Win) `shouldBe` Right input2Doc
            (runTestParse pDocument input2Unix) `shouldBe` Right input2Doc
        
        it "commit to front matter after a single ---" $ do
            -- If the file contains a valid front matter start, the parser may not
            -- fall back to parsing this part of the file as normal file content.
            -- If the front matter that follows is invalid, the parser must fail entirely.
            let input = "---\ntitle: This blog has a front matter that does not end.\n"
            (runTestParse pDocument input) `shouldSatisfy` isLeft
            

runTestParse :: DocParser a -> Text -> Either String a
runTestParse parser input =
  case runParser parser "<file>" input of
    Right x -> Right x
    Left err -> Left (errorBundlePretty err)
