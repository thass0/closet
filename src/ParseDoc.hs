module ParseDoc
  ( Doc (..)
  , FrontMatter
  , Prog (..)
  , Stmt (..)
  , Expr (..)
  , ImmExpr (..)
  , StringyImmExpr (..)
  , NumberyImmExpr (..)
  , Var
  , StringLiteral
  , Number
  , FilterExpr (..)
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

data Expr = Expr ImmExpr [FilterExpr]
  deriving (Show, Eq)

data ImmExpr
  = ImmVar Var
  | ImmStringLiteral StringLiteral
  | ImmNumber Number
  deriving (Show, Eq)

data StringyImmExpr = SImmVar Var | SImmStringLiteral StringLiteral
  deriving (Show, Eq)

data NumberyImmExpr = NImmVar Var | NImmNumber Number
  deriving (Show, Eq)

type StringLiteral = Text
type Number = Int  -- ^ TODO: There are floats and integers in liquid.
type Var = [Ident]

data FilterExpr
  -- * Numeric filters
  = FilterPlus NumberyImmExpr
  | FilterMinus NumberyImmExpr
  | FilterTimes NumberyImmExpr
  | FilterDividedBy NumberyImmExpr
  | FilterModulo NumberyImmExpr
  | FilterAtLeast NumberyImmExpr
  | FilterAtMost NumberyImmExpr
  | FilterAbs
  | FilterCeil
  | FilterFloor
  | FilterRound
  -- * Array and object filters
  | FilterAppend StringyImmExpr
  | FilterConcat Var
  | FilterFirst
  | FilterLast
  | FilterJoin StringyImmExpr
  | FilterReverse
  | FilterSortNatural
  | FilterSort
  | FilterMap StringyImmExpr
  | FilterCompact
  | FilterSum (Maybe StringyImmExpr)
  | FilterUniq
  | FilterWhere StringyImmExpr (Maybe StringyImmExpr)
  -- * String filters
  | FilterCapitalize
  | FilterUpcase
  | FilterDowncase
  | FilterLStrip
  | FilterRStrip
  | FilterPrepend StringyImmExpr
  | FilterReplace StringyImmExpr StringyImmExpr
  | FilterReplaceFirst StringyImmExpr StringyImmExpr
  | FilterRemove StringyImmExpr
  | FilterRemoveFirst StringyImmExpr
  | FilterSize
  | FilterSlice NumberyImmExpr (Maybe NumberyImmExpr)
  | FilterSplit StringyImmExpr
  | FilterStrip
  | FilterStripHtml
  | FilterStripNewlines
  | FilterEscape
  | FilterEscapeOnce
  | FilterNewlineToBr
  | FilterTruncate NumberyImmExpr (Maybe StringyImmExpr)
  | FilterTruncateWords NumberyImmExpr (Maybe StringyImmExpr)
  | FilterUrlDecode
  | FilterUrlEncode
  | FilterDate StringyImmExpr
  -- * Miscellaneous filters
  | FilterDefault ImmExpr
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

-- ** Variables

pIdent :: Parser Ident
pIdent = pIdentInner <&> pack
  where
    pIdentInner = (:) <$> (letterChar <|> char '_')  -- [A-Za-z_]
                      <*> many (alphaNumChar <|> char '_')  -- [A-Za-z0-9_]*
                      <?> "identifier"  -- Label

pIdents :: Parser [Ident]
pIdents =  sepBy1 pIdent (string ".")

pVar :: Parser Var
pVar = pIdents

-- ** Numbers

pInteger :: Parser Int
pInteger = pIntegerInner <?> "number"
  where pIntegerInner = do
          minusSign <- optional (char '-')
          value <- pLexeme L.decimal
          case minusSign of
            Just _ -> pure $ negate value
            Nothing -> pure value

pNumber :: Parser Number
pNumber = pInteger

-- ** String literals

pStringLiteral :: Parser Text
pStringLiteral = startChar *> bodyClosed <&> pack
  where
    startChar = char '\"' <?> "start of string"
    bodyClosed = manyTill L.charLiteral endChar <?> "body of string"
    endChar = char '\"' <?> "end of string"

-- ** Immediate expressions

pImmExpr :: Parser ImmExpr
pImmExpr = choice $ try <$>
  [ pVar <&> ImmVar
  , pNumber <&> ImmNumber
  , pStringLiteral <&> ImmStringLiteral
  ]

pStringyImmExpr :: Parser StringyImmExpr
pStringyImmExpr = choice $ try <$>
  [ pVar <&> SImmVar
  , pStringLiteral <&> SImmStringLiteral
  ]

pNumberyImmExpr :: Parser NumberyImmExpr
pNumberyImmExpr = choice $ try <$>
  [ pVar <&> NImmVar
  , pNumber <&> NImmNumber
  ]


pFilterExpr :: Parser FilterExpr
pFilterExpr = choice $ try <$>
  [ -- * Numeric filters
    withColon "plus" >> pNumberyImmExpr <&> FilterPlus
  , withColon "minus" >> pNumberyImmExpr <&> FilterMinus
  , withColon "times" >> pNumberyImmExpr <&> FilterTimes
  , withColon "divided_by" >> pNumberyImmExpr <&> FilterDividedBy
  , withColon "modulo" >> pNumberyImmExpr <&> FilterModulo
  , withColon "at_least" >> pNumberyImmExpr <&> FilterAtLeast
  , withColon "at_most" >> pNumberyImmExpr <&> FilterAtMost
  , string "abs" $> FilterAbs
  , string "ceil" $> FilterCeil
  , string "floor" $> FilterFloor
  , string "round" $> FilterRound
  -- * Array and object filters
  , withColon "append" >> pStringyImmExpr <&> FilterAppend
  , withColon "concat" >> pVar <&> FilterConcat
  , string "first" $> FilterFirst
  , string "last" $> FilterLast
  , withColon "join" >> pStringyImmExpr <&> FilterJoin
  , string "reverse" $> FilterReverse
  , string "sort_natural" $> FilterSortNatural
  , string "sort" $> FilterSort
  , withColon "map" >> pStringyImmExpr <&> FilterMap
  , string "compact" $> FilterCompact
  , withColon "sum" >> pStringyImmExpr <&> Just <&> FilterSum
  , string "sum" $> FilterSum Nothing
  , string "uniq" $> FilterUniq
  , withColon "where" >> pStringWithMaybeString <&> uncurry FilterWhere
  -- * String filters
  , string "capitalize" $> FilterCapitalize
  , string "upcase" $> FilterUpcase
  , string "downcase" $> FilterDowncase
  , string "lstrip" $> FilterLStrip
  , string "rstrip" $> FilterRStrip
  , withColon "prepend" >> pStringyImmExpr <&> FilterPrepend
  , withColon "replace_first" >> pTwoStrings <&> uncurry FilterReplaceFirst 
  , withColon "replace" >> pTwoStrings <&> uncurry FilterReplace
  , withColon "remove_first" >> pStringyImmExpr <&> FilterRemoveFirst
  , withColon "remove" >> pStringyImmExpr <&> FilterRemove
  , string "size" $> FilterSize
  , withColon "slice" >> pNumberWithMaybeNumber <&> uncurry FilterSlice
  , withColon "split" >> pStringyImmExpr <&> FilterSplit
  , string "strip_newlines" $> FilterStripNewlines
  , string "strip_html" $> FilterStripHtml
  , string "strip" $> FilterStrip
  , string "escape_once" $> FilterEscapeOnce
  , string "escape" $> FilterEscape
  , string "newline_to_br" $> FilterNewlineToBr
  , withColon "truncatewords" >> pNumberWithMaybeString <&> uncurry FilterTruncateWords
  , withColon "truncate" >> pNumberWithMaybeString <&> uncurry FilterTruncate
  , withColon "date" >> pStringyImmExpr <&> FilterDate
  , string "url_decode" $> FilterUrlDecode
  , string "url_encode" $> FilterUrlEncode
  -- * Miscellaneous filters
  , withColon "default" >> pImmExpr <&> FilterDefault
  ]
  where
    pTwoStrings = do
      first <- pLexeme pStringyImmExpr
      void $ pLexeme (char ',')
      second <- pLexeme pStringyImmExpr
      pure (first, second)
    pNumberWithMaybeNumber = do
      first <- pLexeme pNumberyImmExpr
      second <- optional $ do
        void $ pLexeme (char ',')
        pLexeme pNumberyImmExpr
      pure (first, second)
    pNumberWithMaybeString = do
      n <- pLexeme pNumberyImmExpr
      str <- optional $ do
        void $ pLexeme (char ',')
        pLexeme pStringyImmExpr
      pure (n, str)
    pStringWithMaybeString = do
      first <- pLexeme pStringyImmExpr
      second <- optional $ do
        void $ pLexeme (char ',')
        pLexeme pStringyImmExpr
      pure (first, second)
    withColon s = string s >> pLexeme (char ':')

pExpr :: Parser Expr
pExpr = do
  imm <- pLexeme pImmExpr
  filters <- many (pSymbol' "|" >> pLexeme pFilterExpr)
  pure $ Expr imm filters

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
