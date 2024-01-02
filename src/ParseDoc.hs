module ParseDoc
  ( Doc (..)
  , FrontMatter
  , Block (..)
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
import Data.Text (Text, pack, stripEnd, stripStart)
import Data.Void (Void)
import Data.Maybe (fromMaybe, isJust)

data Doc = Doc
  { docFrontMatter :: FrontMatter  -- ^ Front matter of the document.
  , docBlocks :: [Block]  -- ^ Program to generate the document.
  }
  deriving (Show, Eq)

type FrontMatter = Y.Value

data Block
  = Stmt Stmt
  | LiteralContent Text
  deriving (Show, Eq)

data Stmt
  = StmtIf ImmExpr [Block] (Maybe [Block])  -- ^ Predicate Consequent Alternative
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


-- | Store how whitespace should be stripped in and around a tag.
data StripTag a = StripTag
  { beforeTag :: Bool
  , afterTag :: Bool
  , tag :: a
  } deriving (Show, Eq)

instance Functor StripTag where
  fmap f (StripTag b a t) = StripTag b a (f t)

-- | Strip whitespace around the given list of blocks.
stripBlocks :: [StripTag Block] -> [Block]
stripBlocks = stripNestedBlocks False False

-- | Strip whitespace around the given list of blocks.
--   In @stripNestedBlock beforeFirst afterLast blocks@, @globalBefore@
--   and @afterLast@ dictate what how whitespace at the start of the first
--   block and at the end of the last block should be treated.
stripNestedBlocks :: Bool -> Bool -> [StripTag Block] -> [Block]
stripNestedBlocks beforeFirst afterLast bs =
  foldr stripAfterIter [] (foldl stripBeforeIter [] bs)
  where
    stripBeforeIter :: [StripTag Block] -> StripTag Block -> [StripTag Block]
    stripBeforeIter [] (StripTag True after t) = [StripTag False after (stripBlockBefore t)]
    stripBeforeIter [] (StripTag False after t) =
      [StripTag False after (if beforeFirst then stripBlockBefore t else t)]
    stripBeforeIter acc (StripTag True a t) =
      init acc ++ [stripBlockAfter <$> last acc, StripTag False a (stripBlockBefore t)]
    stripBeforeIter acc st@(StripTag False _ _) = acc ++ [st] 

    stripAfterIter :: StripTag Block -> [Block] -> [Block]
    stripAfterIter (StripTag _ True t) [] = [stripBlockAfter t]
    stripAfterIter (StripTag _ False t) [] =
      [if afterLast then stripBlockAfter t else t]
    stripAfterIter (StripTag _ True t) acc =
      stripBlockAfter t : stripBlockBefore (head acc) : tail acc
    stripAfterIter (StripTag _ False t) acc = t : acc

    stripBlockAfter :: Block -> Block
    stripBlockAfter b =
      case b of
        (LiteralContent c) -> LiteralContent (stripEnd c)
        _ -> b

    stripBlockBefore :: Block -> Block
    stripBlockBefore b =
      case b of
        (LiteralContent c) -> LiteralContent (stripStart c)
        _ -> b


pTag :: Text -> Parser a -> Text -> Parser (StripTag a)
pTag open x close = do
  sb <- (string open *> optional (string "-") <* space) <&> isJust
  ret <- x
  sa <- (space *> optional (string "-") <* string close) <&> isJust
  pure StripTag { beforeTag = sb, afterTag = sa, tag = ret }


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
  , (withColon "sum" >> pStringyImmExpr) <&> FilterSum . Just
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

-- * Statements

pIfStmt :: Parser (StripTag Stmt)
pIfStmt = do
  -- TODO: Logical operator (==, and, etc.)
  -- TODO: Elsif
  sTagIf <- pTag "{%" (pSymbol "if" >> pImmExpr) "%}" 
  consequent <- many (try pBlock)
  elseBranch <- optional . try $ do
    sTagElse <- pTag "{%" (string "else") "%}" 
    blocks <- many (try pBlock)
    pure (blocks, sTagElse)
  sTagEndif <- pTag "{%" (string "endif") "%}" 
  case elseBranch of
    Just (alternative, sTagElse) -> do
      let sc = stripNestedBlocks (afterTag sTagIf) (beforeTag sTagElse) consequent
          sa = stripNestedBlocks (afterTag sTagElse) (beforeTag sTagEndif) alternative
      pure $ StripTag (beforeTag sTagIf)
                      (afterTag sTagEndif)
                      (StmtIf (tag sTagIf) sc (Just sa))
    Nothing ->
      let sc = stripNestedBlocks (afterTag sTagIf) (beforeTag sTagEndif) consequent
      in pure $ StripTag (beforeTag sTagIf)
                         (afterTag sTagEndif)
                         (StmtIf (tag sTagIf) sc Nothing)

pStmt :: Parser (StripTag Stmt)
pStmt = choice  -- Note that there is no try here.
  [ pTag "{{" (pExpr <&> StmtExpress) "}}"
  , lookAhead (string "{%" >> optional (string "-") >> space >> pSymbol "if") *> pIfStmt
  ]

stmtLookAhead :: Parser Text
stmtLookAhead = lookAhead $ choice (string <$> ["{{", "{%", "{{-", "{%-"])

pLiteralContent :: Parser Text
pLiteralContent = someTill pAnything endOfContent <&> pack
  where
    endOfContent = (stmtLookAhead $> ()) <|> eof

pBlock :: Parser (StripTag Block)
pBlock = choice
  [ stmtLookAhead >> pStmt <&> \(StripTag b a stmt) -> StripTag b a (Stmt stmt)
  , pLiteralContent <&> \c -> StripTag False False (LiteralContent c)
  ]

-- * Document

pDocument :: Parser Doc
pDocument = do
  -- Parse an optional front matter. If @---@ is found, we commit
  -- to parsing the front matter and don't fall back.
  fm <- optional $ do
    void $ try (lookAhead (space >> pSymbol "---"))
    void space
    pFrontMatter
  blocks' <- many pBlock <&> stripBlocks
  let blocks = if null blocks' then [LiteralContent ""] else blocks'
  pure $ Doc (fromMaybe (Y.object []) fm) blocks