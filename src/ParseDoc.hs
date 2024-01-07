module ParseDoc (
  Doc (..),
  FrontMatter,
  Block (..),
  Tag (..),
  Expr (..),
  BaseExpr (..),
  Var,
  StrLit,
  Number,
  FilterExpr (..),
  pDocument,
  Parser,
) where

import Control.Applicative hiding (many, some)
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import Data.Functor (($>), (<&>))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text, pack, stripEnd, stripStart)
import Data.Void (Void)
import qualified Data.Yaml as Y
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Doc = Doc
  { docFrontMatter :: FrontMatter
  -- ^ Front matter of the document.
  , docBlocks :: [Block]
  -- ^ Program to generate the document.
  }
  deriving (Show, Eq)

type FrontMatter = Y.Value

data Block
  = Tag Tag
  | Cont Text
  deriving (Show, Eq)

data Tag
  = -- | Predicate Consequent Alternatives Final
    TagIf Expr [Block] [(Expr, [Block])] (Maybe [Block])
  | TagFor
  | TagAssign
  | TagUnless
  | TagCase
  | TagCapture
  | TagIncrement
  | TagDecrement
  | TagExpress Expr
  deriving (Show, Eq)

data Expr = Expr BaseExpr [FilterExpr]
  deriving (Show, Eq)

data FilterExpr = FilterExpr Ident [BaseExpr]
  deriving (Show, Eq)

data BaseExpr
  = ImmVar Var
  | ImmStrLit StrLit
  | ImmNum Number
  | ExprAnd BaseExpr BaseExpr -- and
  | ExprOr BaseExpr BaseExpr -- or
  | ExprEq BaseExpr BaseExpr -- ==
  | ExprNeq BaseExpr BaseExpr -- !=
  | ExprGt BaseExpr BaseExpr -- >
  | ExprLt BaseExpr BaseExpr -- <
  | ExprGeq BaseExpr BaseExpr -- >=
  | ExprLeq BaseExpr BaseExpr -- <=
  deriving (Show, Eq)

type StrLit = Text

type Number =
  Int
  -- ^ TODO: There are floats and integers in liquid.

type Var = [Ident]

type Ident = Text

-- * Parser setup

type Parser = Parsec Void Text

-- | Skip one or more white space characters.
spaceConsumer :: Parser ()
spaceConsumer = L.space space1 empty empty

-- | Run the given parser and consume trailing white space (whitespace is optional).
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

-- Raw content that may still need to be formatted by stripping
-- away whitespace in front and at the end of the content. This
-- record stores information on how to format the content along
-- with the content itself.
data Unformatted a = Unformatted
  { beforeTag :: Bool
  , afterTag :: Bool
  , inner :: a
  }
  deriving (Show, Eq)

instance Functor Unformatted where
  fmap f (Unformatted b a t) = Unformatted b a (f t)

-- | Strip whitespace around the given list of blocks.
formatBlocks :: [Unformatted Block] -> [Block]
formatBlocks = formatNestedBlocks False False

-- | Strip whitespace around the given list of blocks.
--   In @stripNestedBlock beforeFirst afterLast blocks@, @globalBefore@
--   and @afterLast@ dictate what how whitespace at the start of the first
--   block and at the end of the last block should be treated.
formatNestedBlocks :: Bool -> Bool -> [Unformatted Block] -> [Block]
formatNestedBlocks beforeFirst afterLast bs =
  foldr stripContEnds [] (foldl stripContStarts [] bs)
  where
    -- Strip whitespace from the start and end of all content blocks.

    stripContStarts
      :: [Unformatted Block] -> Unformatted Block -> [Unformatted Block]
    stripContStarts [] (Unformatted True after t) =
      [Unformatted False after (stripContStart t)]
    stripContStarts [] (Unformatted False after t) =
      [Unformatted False after (if beforeFirst then stripContStart t else t)]
    stripContStarts acc (Unformatted True a t) =
      init acc
        ++ [ stripContEnd <$> last acc
           , Unformatted False a (stripContStart t)
           ]
    stripContStarts acc st@(Unformatted False _ _) = acc ++ [st]

    stripContEnds :: Unformatted Block -> [Block] -> [Block]
    stripContEnds (Unformatted _ True t) [] = stripContEnd t ?: []
    stripContEnds (Unformatted _ False t) [] =
      (if afterLast then stripContEnd t else t) ?: []
    stripContEnds (Unformatted _ True t) acc =
      stripContEnd t ?: (stripContStart (head acc) ?: tail acc)
    stripContEnds (Unformatted _ False t) acc = t ?: acc

    stripContEnd :: Block -> Block
    stripContEnd b =
      case b of
        (Cont c) -> Cont (stripEnd c)
        _ -> b

    stripContStart :: Block -> Block
    stripContStart b =
      case b of
        (Cont c) -> Cont (stripStart c)
        _ -> b

    -- 'Cons' blocks but drop any blocks that are empty content strings.
    (?:) :: Block -> [Block] -> [Block]
    (Cont "") ?: bs' = bs'
    b ?: bs' = b : bs'

inTag :: Text -> Parser a -> Text -> Parser (Unformatted a)
inTag open x close = do
  sb <- (string open *> optional (string "-") <* space) <&> isJust
  ret <- x
  sa <- (space *> optional (string "-") <* string close) <&> isJust
  pure Unformatted{beforeTag = sb, afterTag = sa, inner = ret}

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

-- ** Immediate expressions

pIdent :: Parser Ident
pIdent = pIdentInner <&> pack
  where
    pIdentInner =
      (:)
        <$> (letterChar <|> char '_') -- [A-Za-z_]
        <*> many (alphaNumChar <|> char '_') -- [A-Za-z0-9_]*
        <?> "identifier" -- Label

pIdents :: Parser [Ident]
pIdents = sepBy1 pIdent (string ".")

pVar :: Parser Var
pVar = pIdents

pInteger :: Parser Int
pInteger = pIntegerInner <?> "number"
  where
    pIntegerInner = do
      minusSign <- optional (char '-')
      value <- pLexeme L.decimal
      if isJust minusSign
        then pure (negate value)
        else pure value

pNum :: Parser Number
pNum = pInteger

pStrLit :: Parser Text
pStrLit = startChar *> bodyClosed <&> pack
  where
    startChar = char '\"' <?> "start of string"
    bodyClosed = manyTill L.charLiteral endChar <?> "body of string"
    endChar = char '\"' <?> "end of string"

pImmExpr :: Parser BaseExpr
pImmExpr =
  choice $
    try
      <$> [ pVar <&> ImmVar
          , pNum <&> ImmNum
          , pStrLit <&> ImmStrLit
          ]

-- ** Base expressions

type BaseOp = BaseExpr -> BaseExpr -> BaseExpr

pBaseExpr :: Parser BaseExpr
pBaseExpr = do
  lhs <- pLexeme pImmExpr
  rhss <- many $ (,) <$> pOperand <*> pLexeme pImmExpr
  pure $ applyTransformed lhs (foldl (transformParsed lhs) [] rhss)
  where
    pOperand :: Parser BaseOp
    pOperand =
      choice
        [ pSymbol "and" $> ExprAnd
        , pSymbol "or" $> ExprOr
        ]

    -- Example of how a right-associative expression is transformed:
    --
    -- Input:            "a and b or c and d"
    -- Parsed:           lhs=a, rhss=[(and, b), (or, c), (and, d)]
    -- transformParsed:  [((and, a), b), ((or, b), c), ((and, c), d)]
    -- applyTransformed: (and a (or b (and c d)))

    transformParsed
      :: BaseExpr
      -> [((BaseOp, BaseExpr), BaseExpr)]
      -> (BaseOp, BaseExpr)
      -> [((BaseOp, BaseExpr), BaseExpr)]
    transformParsed lhs [] (newOp, newRhs) = [((newOp, lhs), newRhs)]
    transformParsed _ acc (newOp, newRhs) =
      let (oldPair, newLhs) = last acc
          acc' = init acc
       in acc' ++ [(oldPair, newLhs), ((newOp, newLhs), newRhs)]

    applyTransformed
      :: BaseExpr
      -> [((BaseOp, BaseExpr), BaseExpr)]
      -> BaseExpr
    applyTransformed lhs t = fromMaybe lhs (foldr applyOps Nothing t)

    applyOps
      :: ((BaseOp, BaseExpr), BaseExpr)
      -> Maybe BaseExpr
      -> Maybe BaseExpr
    applyOps ((op, lhs), initRhs) Nothing = Just (op lhs initRhs)
    applyOps ((op, lhs), _) (Just rhs) = Just (op lhs rhs)

pFilterExpr :: Parser FilterExpr
pFilterExpr = do
  ident <- pLexeme pIdent
  args <- optional . try $ do
    -- Optionally parse filter argument. Once a colon
    -- was seen, at least one argument must follow.
    void $ pSymbol' ":"
    sepBy1 pBaseExpr (pSymbol' ",")
  case args of
    Just args' -> pure $ FilterExpr ident args'
    Nothing -> pure $ FilterExpr ident []

pExpr :: Parser Expr
pExpr = do
  base <- pLexeme pBaseExpr
  filters <- many (pSymbol' "|" >> pLexeme pFilterExpr)
  pure $ Expr base filters

-- * Tags

pIfTag :: Parser (Unformatted Tag)
pIfTag = do
  sTagIf <- inTag "{%" (pSymbol "if" >> pExpr) "%}"
  conseq <- many (try pBlock)

  elsifBranches <- many . try $ do
    sTagElsif <- inTag "{%" (pSymbol "elsif" >> pExpr) "%}"
    blocks <- many (try pBlock)
    pure (sTagElsif, blocks)

  elseBranch <- optional . try $ do
    sTagElse <- inTag "{%" (string "else") "%}"
    blocks <- many (try pBlock)
    pure (sTagElse, blocks)

  sTagEndif <- inTag "{%" (string "endif") "%}"

  let elsifTags = fst <$> elsifBranches
  case elseBranch of
    Just (sTagElse, final) -> do
      let fmtAlts = formatElsifBlocks (beforeTag sTagElse) elsifBranches
      let beforeConseq = afterTag sTagIf
          afterConseq = case elsifTags of
            (firstTag : _) -> beforeTag firstTag
            _ -> beforeTag sTagElse
          fmtConseq = formatNestedBlocks beforeConseq afterConseq conseq
      let beforeFinal = case elsifTags of
            [] -> afterTag sTagElse
            eb -> let lastTag = last eb in afterTag lastTag
          afterFinal = beforeTag sTagEndif
          fmtFinal = formatNestedBlocks beforeFinal afterFinal final
      pure $
        Unformatted
          (beforeTag sTagIf)
          (afterTag sTagEndif)
          (TagIf (inner sTagIf) fmtConseq fmtAlts (Just fmtFinal))
    Nothing ->
      let fmtAlts = formatElsifBlocks (beforeTag sTagEndif) elsifBranches
          beforeConseq = afterTag sTagIf
          afterConseq = case elsifTags of
            (firstTag : _) -> beforeTag firstTag
            _ -> beforeTag sTagEndif
          fmtConseq = formatNestedBlocks beforeConseq afterConseq conseq
       in pure $
            Unformatted
              (beforeTag sTagIf)
              (afterTag sTagEndif)
              (TagIf (inner sTagIf) fmtConseq fmtAlts Nothing)

-- | Use @formatElsifBlocks stripAfterLast parsedElsifs@ to format parsed 'elsif' blocks.
formatElsifBlocks
  :: Bool -> [(Unformatted Expr, [Unformatted Block])] -> [(Expr, [Block])]
formatElsifBlocks afterLast blocks =
  let info = foldr collectFormatInfo [] blocks
   in formatWithInfo <$> info
  where
    -- Elsif alternatives are parsed as a list of pairs of a predicate and a
    -- list of blocks to execute if the predicate is true. Those blocks (called
    -- an alternative) need to know if white space should be stripped away
    -- _after_ their predicate and if white space should be stripped _before_
    -- the tag that follows. The tag that follows the last alternative is not
    -- part of the list anymore. 'collectFormatInfo' moves all information about
    -- how to format an alternative into the list element of that alternative itself.
    collectFormatInfo
      :: (Unformatted Expr, [Unformatted Block])
      -- \^ Current.
      -> [(Unformatted Expr, Unformatted [Unformatted Block])]
      -- \^ Accumulator
      -> [(Unformatted Expr, Unformatted [Unformatted Block])]
    collectFormatInfo (expr, alt) [] = [(expr, Unformatted (afterTag expr) afterLast alt)]
    collectFormatInfo (expr, alt) ((afterExpr, afterAlt) : acc) =
      (expr, Unformatted (afterTag expr) (beforeTag afterExpr) alt)
        : (afterExpr, afterAlt)
        : acc

    formatWithInfo
      :: (Unformatted Expr, Unformatted [Unformatted Block]) -> (Expr, [Block])
    formatWithInfo (Unformatted _ _ expr, Unformatted beforeAlt afterAlt alt) =
      (expr, formatNestedBlocks beforeAlt afterAlt alt)

curlyTagLookAhead :: Text -> Parser Text
curlyTagLookAhead name = lookAhead (string "{%" >> optional (string "-") >> space >> pSymbol name)

pTag :: Parser (Unformatted Tag)
pTag =
  choice -- Note that there is no 'try' here.
    [ inTag "{{" (pExpr <&> TagExpress) "}}"
    , curlyTagLookAhead "if" >> pIfTag
    ]

tagLookAhead :: Parser Text
tagLookAhead = lookAhead $ choice (string <$> ["{{", "{%", "{{-", "{%-"])

pCont :: Parser Text
pCont = someTill pAnything endOfContent <&> pack
  where
    endOfContent = tagLookAhead' <|> eof
    tagLookAhead' = tagLookAhead $> () -- Type stuff only.

pBlock :: Parser (Unformatted Block)
pBlock =
  choice
    [ tagLookAhead >> pTag <&> fmap Tag
    , pCont <&> Unformatted False False . Cont
    ]

-- * Document

pDocument :: Parser Doc
pDocument = do
  -- Parse an optional front matter. If '---' is found, we commit
  -- to parsing the front matter and don't fall back.
  fm <- optional $ do
    void $ try (lookAhead (space >> pSymbol "---"))
    void space
    pFrontMatter
  blocks' <- many pBlock <&> formatBlocks
  let blocks = if null blocks' then [Cont ""] else blocks'
  pure $ Doc (fromMaybe (Y.object []) fm) blocks
