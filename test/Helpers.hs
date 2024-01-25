module Helpers (runTestParse, runDocParse, runDocParse', doc) where

import Data.Aeson.KeyMap as Aeson.KeyMap
import Data.Text (Text)
import qualified ParseDoc
import Text.Megaparsec

runDocParse :: Text -> Either String ParseDoc.Doc
runDocParse = runTestParse (ParseDoc.pDocument <* eof)

runTestParse :: ParseDoc.Parser a -> Text -> Either String a
runTestParse parser input =
  case runParser parser "<in>" input of
    Right x -> Right x
    Left err -> Left (errorBundlePretty err)

runDocParse' :: Text -> ParseDoc.Doc
runDocParse' input =
  case runDocParse input of
    Right x -> x
    Left e -> error e

doc :: [ParseDoc.Block] -> ParseDoc.Doc
doc = ParseDoc.Doc Aeson.KeyMap.empty
