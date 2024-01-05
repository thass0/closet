module ParseDoc
  ( Doc (..)
  , FrontMatter
  , Block (..)
  , Stmt (..)
  , Expr (..)
  , immVar
  , BaseExpr (..)
  , Stringy (..)
  , Numbery (..)
  , Var
  , StrLit
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
  = StmtIf Expr [Block] [(Expr, [Block])] (Maybe [Block])  -- ^ Predicate Consequent Alternatives Final
  | StmtFor
  | StmtAssign
  | StmtUnless
  | StmtCase
  | StmtCapture
  | StmtIncrement
  | StmtDecrement
  | StmtExpress Expr
  deriving (Show, Eq)

data Expr = Expr BaseExpr [FilterExpr]
  deriving (Show, Eq)

immVar :: Var -> Expr
immVar v = Expr (ImmVar v) []

data BaseExpr
  = ImmVar Var 
  | ImmStrLit StrLit
  | ImmNum Number
  | ExprAnd Expr Expr  -- and
  | ExprOr Expr Expr  -- or 
  | ExprEq Expr Expr  -- ==
  | ExprNeq Expr Expr  -- !=
  | ExprGt Expr Expr  -- >
  | ExprLt Expr Expr  -- <
  | ExprGeq Expr Expr  -- >=
  | ExprLeq Expr Expr  -- <=
  deriving (Show, Eq)

data Stringy = StringyVar Var | StringyLit StrLit 
  deriving (Show, Eq)

data Numbery = NumberyVar Var | NumberyNum Number
  deriving (Show, Eq)

type StrLit = Text
type Number = Int  -- ^ TODO: There are floats and integers in liquid.
type Var = [Ident]

data FilterExpr
  -- * Numeric filters
  = FilterPlus Numbery
  | FilterMinus Numbery
  | FilterTimes Numbery
  | FilterDividedBy Numbery
  | FilterModulo Numbery
  | FilterAtLeast Numbery
  | FilterAtMost Numbery
  | FilterAbs
  | FilterCeil
  | FilterFloor
  | FilterRound
  -- * Array and object filters
  | FilterAppend Stringy
  | FilterConcat Var
  | FilterFirst
  | FilterLast
  | FilterJoin Stringy
  | FilterReverse
  | FilterSortNatural
  | FilterSort
  | FilterMap Stringy
  | FilterCompact
  | FilterSum (Maybe Stringy)
  | FilterUniq
  | FilterWhere Stringy (Maybe Stringy)
  -- * String filters
  | FilterCapitalize
  | FilterUpcase
  | FilterDowncase
  | FilterLStrip
  | FilterRStrip
  | FilterPrepend Stringy
  | FilterReplace Stringy Stringy
  | FilterReplaceFirst Stringy Stringy
  | FilterRemove Stringy
  | FilterRemoveFirst Stringy
  | FilterSize
  | FilterSlice Numbery (Maybe Numbery)
  | FilterSplit Stringy
  | FilterStrip
  | FilterStripHtml
  | FilterStripNewlines
  | FilterEscape
  | FilterEscapeOnce
  | FilterNewlineToBr
  | FilterTruncate Numbery (Maybe Stringy)
  | FilterTruncateWords Numbery (Maybe Stringy)
  | FilterUrlDecode
  | FilterUrlEncode
  | FilterDate Stringy
  -- * Miscellaneous filters
  | FilterDefault BaseExpr
  deriving (Show, Eq)

type Ident = Text


-- * Parser setup

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
    stripAfterIter (StripTag _ True t) [] = stripBlockAfter t ?: []
    stripAfterIter (StripTag _ False t) [] =
      (if afterLast then stripBlockAfter t else t) ?: []
    stripAfterIter (StripTag _ True t) acc =
      stripBlockAfter t ?: (stripBlockBefore (head acc) ?: tail acc)
    stripAfterIter (StripTag _ False t) acc = t ?: acc

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
    
    -- Cons blocks but drop any blocks that's an empty literal
    -- content string. Used in 'stripAfterIter' to drop all content
    -- that was purely made of whitespace and stripped.
    (?:) :: Block -> [Block] -> [Block]
    (LiteralContent "") ?: bs' = bs'
    b ?: bs' = b : bs'


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

pBaseExpr :: Parser BaseExpr
pBaseExpr = choice $ try <$>
  [ pVar <&> ImmVar
  , pNumber <&> ImmNum
  , pStringLiteral <&> ImmStrLit
  ]

pStringy :: Parser Stringy
pStringy = choice $ try <$>
  [ pVar <&> StringyVar
  , pStringLiteral <&> StringyLit
  ]

pNumbery :: Parser Numbery
pNumbery = choice $ try <$>
  [ pVar <&> NumberyVar
  , pNumber <&> NumberyNum
  ]


pFilterExpr :: Parser FilterExpr
pFilterExpr = choice $ try <$>
  [ -- * Numeric filters
    withColon "plus" >> pNumbery <&> FilterPlus
  , withColon "minus" >> pNumbery <&> FilterMinus
  , withColon "times" >> pNumbery <&> FilterTimes
  , withColon "divided_by" >> pNumbery <&> FilterDividedBy
  , withColon "modulo" >> pNumbery <&> FilterModulo
  , withColon "at_least" >> pNumbery <&> FilterAtLeast
  , withColon "at_most" >> pNumbery <&> FilterAtMost
  , string "abs" $> FilterAbs
  , string "ceil" $> FilterCeil
  , string "floor" $> FilterFloor
  , string "round" $> FilterRound
  -- * Array and object filters
  , withColon "append" >> pStringy <&> FilterAppend
  , withColon "concat" >> pVar <&> FilterConcat
  , string "first" $> FilterFirst
  , string "last" $> FilterLast
  , withColon "join" >> pStringy <&> FilterJoin
  , string "reverse" $> FilterReverse
  , string "sort_natural" $> FilterSortNatural
  , string "sort" $> FilterSort
  , withColon "map" >> pStringy <&> FilterMap
  , string "compact" $> FilterCompact
  , (withColon "sum" >> pStringy) <&> FilterSum . Just
  , string "sum" $> FilterSum Nothing
  , string "uniq" $> FilterUniq
  , withColon "where" >> pStringWithMaybeString <&> uncurry FilterWhere
  -- * String filters
  , string "capitalize" $> FilterCapitalize
  , string "upcase" $> FilterUpcase
  , string "downcase" $> FilterDowncase
  , string "lstrip" $> FilterLStrip
  , string "rstrip" $> FilterRStrip
  , withColon "prepend" >> pStringy <&> FilterPrepend
  , withColon "replace_first" >> pTwoStrings <&> uncurry FilterReplaceFirst 
  , withColon "replace" >> pTwoStrings <&> uncurry FilterReplace
  , withColon "remove_first" >> pStringy <&> FilterRemoveFirst
  , withColon "remove" >> pStringy <&> FilterRemove
  , string "size" $> FilterSize
  , withColon "slice" >> pNumberWithMaybeNumber <&> uncurry FilterSlice
  , withColon "split" >> pStringy <&> FilterSplit
  , string "strip_newlines" $> FilterStripNewlines
  , string "strip_html" $> FilterStripHtml
  , string "strip" $> FilterStrip
  , string "escape_once" $> FilterEscapeOnce
  , string "escape" $> FilterEscape
  , string "newline_to_br" $> FilterNewlineToBr
  , withColon "truncatewords" >> pNumberWithMaybeString <&> uncurry FilterTruncateWords
  , withColon "truncate" >> pNumberWithMaybeString <&> uncurry FilterTruncate
  , withColon "date" >> pStringy <&> FilterDate
  , string "url_decode" $> FilterUrlDecode
  , string "url_encode" $> FilterUrlEncode
  -- * Miscellaneous filters
  , withColon "default" >> pBaseExpr <&> FilterDefault
  ]
  where
    pTwoStrings = do
      first <- pLexeme pStringy
      void $ pLexeme (char ',')
      second <- pLexeme pStringy
      pure (first, second)
    pNumberWithMaybeNumber = do
      first <- pLexeme pNumbery
      second <- optional $ do
        void $ pLexeme (char ',')
        pLexeme pNumbery
      pure (first, second)
    pNumberWithMaybeString = do
      n <- pLexeme pNumbery
      str <- optional $ do
        void $ pLexeme (char ',')
        pLexeme pStringy
      pure (n, str)
    pStringWithMaybeString = do
      first <- pLexeme pStringy
      second <- optional $ do
        void $ pLexeme (char ',')
        pLexeme pStringy
      pure (first, second)
    withColon s = string s >> pLexeme (char ':')

pExpr :: Parser Expr
pExpr = do
  imm <- pLexeme pBaseExpr
  filters <- many (pSymbol' "|" >> pLexeme pFilterExpr)
  pure $ Expr imm filters

-- * Statements

pIfStmt :: Parser (StripTag Stmt)
pIfStmt = do
  -- TODO: Logical operator (==, and, etc.)
  sTagIf <- pTag "{%" (pSymbol "if" >> pExpr) "%}" 
  consequent <- many (try pBlock)

  elsifBranches <- many . try $ do
    sTagElsif <- pTag "{%" (pSymbol "elsif" >> pExpr) "%}"
    blocks <- many (try pBlock)
    pure (sTagElsif, blocks)

  elseBranch <- optional . try $ do
    sTagElse <- pTag "{%" (string "else") "%}" 
    blocks <- many (try pBlock)
    pure (sTagElse, blocks)

  sTagEndif <- pTag "{%" (string "endif") "%}" 

  let elsifTags = fst <$> elsifBranches
  case elseBranch of
    Just (sTagElse, final) -> do
      let strippedAlts = stripParsedElsifs (beforeTag sTagElse) elsifBranches
      let beforeCons = afterTag sTagIf
          afterCons = case elsifTags of
                        (firstTag : _) -> beforeTag firstTag
                        _ -> beforeTag sTagElse
          strippedCons = stripNestedBlocks beforeCons afterCons consequent
      let beforeFinal = case elsifTags of
                          [] -> afterTag sTagElse
                          eb -> let lastTag = last eb in afterTag lastTag
          afterFinal = beforeTag sTagEndif
          strippedFinal = stripNestedBlocks beforeFinal afterFinal final
      pure $ StripTag (beforeTag sTagIf)
                      (afterTag sTagEndif)
                      (StmtIf (tag sTagIf) strippedCons strippedAlts (Just strippedFinal))
    Nothing ->
      let strippedAlts = stripParsedElsifs (beforeTag sTagEndif) elsifBranches
          beforeCons = afterTag sTagIf
          afterCons = case elsifTags of
                        (firstTag : _) -> beforeTag firstTag
                        _ -> beforeTag sTagEndif
          strippedCons = stripNestedBlocks beforeCons afterCons consequent
      in pure $ StripTag (beforeTag sTagIf)
                         (afterTag sTagEndif)
                         (StmtIf (tag sTagIf) strippedCons strippedAlts Nothing)

-- | @stripParsedElsifs stripAfterLast parsedElsifs@ strips whitespace from elsifs.
stripParsedElsifs :: Bool -> [(StripTag Expr, [StripTag Block])] -> [(Expr, [Block])]
stripParsedElsifs afterLast blocks =
  let collected = foldr collectStripInfo [] blocks
  in stripWithInfo <$> collected
  where
    -- Elsif alternatives are parsed as a list of pairs of a predicate and a list of blocks to execute
    -- if the predicate is true. Those blocks (called an alternative) need to know if white space
    -- should be stripped _after_ their predicate and if white space should be tripped _before_
    -- the tag that follows. The tag that follows the last alternative is not part of the list anymore.
    -- This moves all information about how to strip an alternative into the list element of that
    -- alternative itself. Use it as a foldr.
    collectStripInfo
      :: (StripTag Expr, [StripTag Block])  -- ^ Current.
      -> [(StripTag Expr, StripTag [StripTag Block])]  -- ^ Accumulator
      -> [(StripTag Expr, StripTag [StripTag Block])]
    collectStripInfo (expr, alt) [] = [(expr, StripTag (afterTag expr) afterLast alt)]
    collectStripInfo (expr, alt) ((afterExpr, afterAlt):acc)
      = (expr, StripTag (afterTag expr) (beforeTag afterExpr) alt)
      : (afterExpr, afterAlt)
      : acc
 
    stripWithInfo :: (StripTag Expr, StripTag [StripTag Block]) -> (Expr, [Block])
    stripWithInfo (StripTag _ _ expr, StripTag beforeAlt afterAlt alt) =
      (expr, stripNestedBlocks beforeAlt afterAlt alt)

stmtLookAhead :: Text -> Parser Text
stmtLookAhead name = lookAhead (string "{%" >> optional (string "-") >> space >> pSymbol name)

pStmt :: Parser (StripTag Stmt)
pStmt = choice  -- Note that there is no try here.
  [ pTag "{{" (pExpr <&> StmtExpress) "}}"
  , stmtLookAhead "if" >> pIfStmt
  ]

tagLookAhead :: Parser Text
tagLookAhead = lookAhead $ choice (string <$> ["{{", "{%", "{{-", "{%-"])

pLiteralContent :: Parser Text
pLiteralContent = someTill pAnything endOfContent <&> pack
  where
    endOfContent = (tagLookAhead $> ()) <|> eof

pBlock :: Parser (StripTag Block)
pBlock = choice
  [ tagLookAhead >> pStmt <&> fmap Stmt
  , pLiteralContent <&> StripTag False False . LiteralContent
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
