module Doc
  ( Doc (..)
  , DocFrontMatter 
  , DocProg (..)
  , DocStmt (..)
  , pDocument
  , DocParser
  ) where

import qualified Data.Yaml as Y
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Control.Applicative hiding (many, some)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Data.Text hiding (empty, singleton)
import Data.Void (Void)
import Data.Maybe (fromMaybe)

data Doc = Doc
  { docFrontMatter :: DocFrontMatter  -- ^ Front matter of the document.
  , docProg :: [DocProg]  -- ^ Program to generate the document.
  }
  deriving (Show, Eq)

type DocFrontMatter = Y.Value

data DocProg
  = DocStmt DocStmt
  | DocLiteral Text
  deriving (Show, Eq)

data DocStmt
  = StmtIf
  | StmtFor
  | StmtAssign
  | StmtUnless
  | StmtCase
  | StmtCapture
  | StmtIncrement
  | StmtDecrement
  | StmtExpress
  deriving (Show, Eq)

type DocParser = Parsec Void Text

-- | Skip one or more white space characters.
spaceConsumer :: DocParser ()
spaceConsumer = L.space space1 empty empty

-- | Run the given parser and consume trailing white space.
pLexeme :: DocParser a -> DocParser a
pLexeme = L.lexeme spaceConsumer

-- | Parse the given symbol. It must be followed by one or
--   more white space characters.
pSymbol :: Text -> DocParser Text
pSymbol s = do
  void $ string s
  pLexeme space1
  pure s

-- | Parse the given symbol. White space that follows the symbol
--   is consumed.
pSymbol' :: Text -> DocParser Text
pSymbol' = L.symbol spaceConsumer

-- | Parse a document's front matter.
pFrontMatter :: DocParser DocFrontMatter
pFrontMatter = do
  void $ pSymbol "---"
  dat <- someTill (satisfy (const True)) (try (eol *> string "---" *> eol))
  case Y.decodeEither' (BS.pack dat) of
    Left err -> error $ Y.prettyPrintParseException err
    Right obj -> pure obj

pDocument :: DocParser Doc
pDocument = do
  -- Consume initial space once. All other white space is
  -- picked up by `pSymbol` and `pLexeme`.
  void Text.Megaparsec.Char.space
  -- Parse an optional front matter.
  fm <- optional $ do
    void $ lookAhead (pSymbol "---")
    pFrontMatter
  cont <- manyTill (satisfy (const True)) eof
  pure $ Doc (fromMaybe (Y.object []) fm) [DocLiteral $ pack cont]
