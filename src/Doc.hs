module Doc
  ( Doc (..)
  , FrontMatter
  , Prog (..)
  , Stmt (..)
  , Expr (..)
  , pDocument
  , Parser
  ) where

import qualified Data.Yaml as Y
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Control.Applicative hiding (many, some)
import Data.Functor ((<&>), ($>))
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text hiding (empty, singleton, null)
import Data.Void (Void)
import Data.Maybe (fromMaybe)

data Doc = Doc
  { docFrontMatter :: FrontMatter  -- ^ Front matter of the document.
  , docProg :: [Prog]  -- ^ Program to generate the document.
  }
  deriving (Show, Eq)

type FrontMatter = Y.Value

data Prog
  = Stmt Stmt
  | LiteralContent Text
  deriving (Show, Eq)

data Stmt
  = StmtIf
  | StmtFor
  | StmtAssign
  | StmtUnless
  | StmtCase
  | StmtCapture
  | StmtIncrement
  | StmtDecrement
  | StmtExpress Expr
  deriving (Show, Eq)

-- | TODO: Filters
data Expr
  = Var [Ident]
  | StringLiteral Text
  | Number Int  -- ^ TODO: There are floats and integers in liquid.
  deriving (Show, Eq)

type Ident = Text

type Parser = Parsec Void Text

-- | Skip one or more white space characters.
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

-- | Run the given parser and consume trailing white space.
pLexeme :: Parser a -> Parser a
pLexeme = L.lexeme spaceConsumer

-- | Consume any white space in the input.
pSpace :: Parser ()
pSpace = pLexeme space

-- | Parse the given symbol. It must be followed by one or
--   more white space characters.
pSymbol :: Text -> Parser Text
pSymbol s = do
  void $ string s
  pLexeme space1
  pure s

-- | Parse the given symbol. White space that follows the symbol
--   is consumed.
pSymbol' :: Text -> Parser Text
pSymbol' = L.symbol spaceConsumer

-- | Accept any character. Used together with @manyTill@.
pAnything :: Parser Char
pAnything = satisfy (const True)


-- * Front Matter

-- | Parse a document's front matter.
pFrontMatter :: Parser FrontMatter
pFrontMatter = do
  void $ pSymbol "---"
  dat <- someTill pAnything (try (eol *> string "---" *> eol))
  case Y.decodeEither' (BS.pack dat) of
    Left err -> error $ Y.prettyPrintParseException err
    Right obj -> pure obj


-- * Expressions

pIdent :: Parser Ident
pIdent = pIdentInner <&> pack
  where
    pIdentInner = (:) <$> (letterChar <|> char '_')  -- [A-Za-z_]
                      <*> many (alphaNumChar <|> char '_')  -- [A-Za-z0-9_]*
                      <?> "identifier"  -- Label

pIdents :: Parser [Ident]
pIdents =  sepBy1 pIdent (string ".")

pInteger :: Parser Int
pInteger = pLexeme L.decimal <?> "number"

pString :: Parser Text
pString = startChar *> bodyClosed <&> pack
  where
    startChar = char '\"' <?> "start of string"
    bodyClosed = manyTill L.charLiteral endChar <?> "body of string"
    endChar = char '\"' <?> "end of string"

pExpr :: Parser Expr
pExpr = choice $ try <$>
  [ pIdents <&> Var
  , pInteger <&> Number
  , pString <&> StringLiteral
  ]


-- * Program content

pStmt :: Parser Stmt
pStmt = between (string "{{" >> pSpace) (pSpace >> string "}}") (pExpr <&> StmtExpress)

stmtLookAhead :: Parser Text
stmtLookAhead = lookAhead $ choice (string <$> ["{{", "{%", "{{-", "{%-"])

pLiteralContent :: Parser Text
pLiteralContent = someTill pAnything endOfContent <&> pack
  where
    endOfContent = (stmtLookAhead $> ()) <|> eof

pProgram :: Parser Prog
pProgram = choice
  [ stmtLookAhead >> pStmt <&> Stmt
  , pLiteralContent <&> LiteralContent
  ]

pDocument :: Parser Doc
pDocument = do
  -- Consume initial space once. All other white space is
  -- picked up by @pSymbol@ and @pLexeme@.
  void Text.Megaparsec.Char.space
  -- Parse an optional front matter. If @---@ is found, we commit
  -- to parsing the front matter and don't fall back.
  fm <- optional $ do
    void $ lookAhead (pSymbol "---")
    pFrontMatter
  prog' <- many pProgram
  let prog = if null prog' then [LiteralContent ""] else prog'
  pure $ Doc (fromMaybe (Y.object []) fm) prog
