module EvalDoc (
  Env,
  empty,
  eval,
  Value (..),
  Symbol,
) where

import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.HashMap.Lazy as Map
import Data.List (find)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust, mapMaybe)
import Data.Scientific
import Data.Text (Text, intercalate, isInfixOf, pack)
import qualified Data.Vector as Vector (toList)
import qualified Data.Yaml as Y
import DocModel
import ParseDoc hiding (empty)

data Value
  = Str Text
  | Num Scientific
  | Array [Value]
  | Map SymbolMap
  | Bool Bool
  | Empty
  | Nil (Maybe Ident)
  deriving (Show, Eq)

type SymbolMap = Map.HashMap Symbol Value

type Env = SymbolMap

empty :: Env
empty = Map.empty

showNil :: Maybe Ident -> String
showNil (Just i) = "nil of '" ++ showIdent i ++ "'"
showNil Nothing = "nil"

isEqual :: Value -> Value -> Bool
isEqual v1 v2 =
  case (v1, v2) of
    (Array a, Empty) -> null a
    (Empty, Array a) -> null a
    (Str s, Empty) -> s == ""
    (Empty, Str s) -> s == ""
    (Str s1, Str s2) -> s1 == s2
    (Num n1, Num n2) -> n1 == n2
    (Array a1, Array a2) -> a1 == a2
    (Map m1, Map m2) -> m1 == m2
    (Bool b1, Bool b2) -> b1 == b2
    (Nil ident, _) -> nilErr ident
    (_, Nil ident) -> nilErr ident
    (Empty, Empty) -> True
    _ -> False
  where
    nilErr ident = error $ "cannot compare to " ++ showNil ident

isNotEqual :: Value -> Value -> Bool
isNotEqual a b = not (isEqual a b)

isLessEqual :: Value -> Value -> Bool
isLessEqual v1 v2 =
  case (v1, v2) of
    (Str s1, Str s2) -> s1 <= s2
    (Num n1, Num n2) -> n1 <= n2
    (Array a1, Array a2) -> length a1 <= length a2
    (Map m1, Map m2) -> length m1 <= length m2
    (Bool b1, Bool b2) -> not (b1 && not b2)
    (Nil ident, _) -> nilErr ident
    (_, Nil ident) -> nilErr ident
    (_, _) -> error "cannot order types"
  where
    nilErr ident = error $ "cannot order compared to " ++ showNil ident

isLess :: Value -> Value -> Bool
isLess a b = isNotEqual a b && isLessEqual a b

isGreaterEqual :: Value -> Value -> Bool
isGreaterEqual a b = isEqual a b || not (isLessEqual a b)

isGreater :: Value -> Value -> Bool
isGreater a b = not (isLessEqual a b)

-- Evaluate the truthiness of a given value.
isTruthy :: Value -> Bool
isTruthy v =
  case v of
    Bool b -> b
    Nil _ -> False
    -- Empty string, empty array, and 0 are truthy, too.
    _ -> True

eval :: Env -> Doc -> (Env, Text)
eval env' doc =
  let env = env' <> fromFrontMatter (docFrontMatter doc)
   in foldl evalBlock (env, "") (docBlocks doc)
  where
    evalBlock :: (Env, Text) -> Block -> (Env, Text)
    evalBlock (e, t) (Cont c) = (e, t <> c)
    evalBlock (e, t) (Tag n) =
      case n of
        TagIf branches final ->
          case findBranch e branches of
            Just blocks -> foldl evalBlock (e, t) blocks
            Nothing -> case final of
              Just finalBlocks -> foldl evalBlock (e, t) finalBlocks
              Nothing -> (e, t) -- There's nothing to change.
        TagUnless branches final ->
          case findUnlessBranch e branches of
            Just blocks -> foldl evalBlock (e, t) blocks
            Nothing -> case final of
              Just finalBlocks -> foldl evalBlock (e, t) finalBlocks
              Nothing -> (e, t) -- There's nothing to change.
        TagExpress expr ->
          let r = evalExpr e expr
           in (e, t <> literal r)
        _ -> error "not yet implemented"

    findBranch :: Env -> NE.NonEmpty (Expr, [Block]) -> Maybe [Block]
    findBranch e branches = snd <$> find (isTruthy . evalExpr e . fst) branches

    findUnlessBranch :: Env -> NE.NonEmpty (Expr, [Block]) -> Maybe [Block]
    findUnlessBranch e branches =
      case NE.toList branches of
        [(expr, c)] ->
          if isTruthy (evalExpr e expr)
            then Nothing
            else Just c
        bs -> snd <$> find (isTruthy . evalExpr e . fst) bs

evalExpr :: Env -> Expr -> Value
evalExpr env (Expr base filters) =
  let r = evalBase base
   in foldl (applyFilter env) r filters
  where
    -- Evaluate a base expression.
    evalBase :: BaseExpr -> Value
    evalBase expr =
      case expr of
        ImmVar var -> lookupVar env var
        ImmStrLit str -> Str str
        ImmNum num -> Num num
        ImmBool b -> Bool b
        ImmEmpty -> Empty
        ExprAnd a b ->
          if isTruthy (evalBase a)
            then Bool (isTruthy (evalBase b))
            else Bool False
        ExprOr a b ->
          if isTruthy (evalBase a)
            then Bool True
            else Bool (isTruthy (evalBase b))
        ExprEq a b -> Bool $ isEqual (evalBase a) (evalBase b)
        ExprNeq a b -> Bool $ isNotEqual (evalBase a) (evalBase b)
        ExprGt a b -> Bool $ isGreater (evalBase a) (evalBase b)
        ExprLt a b -> Bool $ isLess (evalBase a) (evalBase b)
        ExprGeq a b -> Bool $ isGreaterEqual (evalBase a) (evalBase b)
        ExprLeq a b -> Bool $ isLessEqual (evalBase a) (evalBase b)
        ExprContains a b ->
          case (evalBase a, evalBase b) of
            (Str super, Str sub) -> Bool (sub `isInfixOf` super)
            (Array strs, str) -> Bool (isJust (find (== str) strs))
            _ -> error "'contains' can only find substrings and strings in arrays"

    -- Lookup a variable in a map.
    lookupVar :: SymbolMap -> Ident -> Value
    lookupVar symMap path =
      case lookupVarInner symMap (NE.toList path) of
        Just v -> v
        Nothing -> Nil (Just path)
      where
        lookupVarInner :: SymbolMap -> [Symbol] -> Maybe Value
        lookupVarInner m [p] = Map.lookup p m
        lookupVarInner m (p : ps) =
          case Map.lookup p m of
            Just (Map m') -> lookupVarInner m' ps
            x -> x
        lookupVarInner _ [] = Nothing

    -- Apply a single filter expression to a value.
    applyFilter :: Env -> Value -> FilterExpr -> Value
    applyFilter _ v (FilterExpr ident args) =
      case (ident, args) of
        ("times", [s]) -> case (v, s) of
          (Num vn, ImmNum sn) -> Num (vn * sn)
          _ -> error "type error in 'times'"
        ("plus", [s]) -> case (v, s) of
          (Num vn, ImmNum sn) -> Num (vn + sn)
          _ -> error "type error in 'plus'"
        ("sum", [s]) -> case (v, s) of
          (Array a, ImmStrLit field) ->
            Num $ sum $ mapMaybe ((onlyNums <$>) . Map.lookup field . onlyMaps) a
          _ -> error "type error in 'sum'"
        ("sum", []) -> case v of
          (Array a) -> Num $ sum $ map onlyNums a
          _ -> error "type error in 'sum'"
        _ -> error "filter not yet implemented"
      where
        onlyMaps :: Value -> SymbolMap
        onlyMaps (Map m) = m
        onlyMaps _ = error "'sum' expected a map"

        onlyNums :: Value -> Scientific
        onlyNums (Num n) = n
        onlyNums _ = error "'sum' expected a number"

-- Convert a value to a literal string that can be
-- added to the output document.
literal :: Value -> Text
literal tlv =
  -- Arrays and maps are not enclosed by parentheses on the top level.
  case tlv of
    Array a -> intercalate ", " (map literal' a)
    Map m -> intercalate ", " (literalMap m)
    tlv' -> literal' tlv'
  where
    literal' v =
      case v of
        Str s -> s
        Num n ->
          pack $ case floatingOrInteger n of
            Left real -> show (real :: Double)
            Right int -> show (int :: Int)
        Array a -> "[" <> intercalate ", " (map literal' a) <> "]"
        Map m -> "{ " <> intercalate ", " (literalMap m) <> " }"
        Bool True -> "true"
        Bool False -> "false"
        Empty -> ""
        Nil ident -> error $ showNil ident ++ " has no value"

    literalMap :: SymbolMap -> [Text]
    literalMap = map (\(k, v') -> k <> ": " <> literal' v') . Map.toList

fromFrontMatter :: Aeson.KeyMap.KeyMap Y.Value -> SymbolMap
fromFrontMatter = fromYamlMap
  where
    fromYaml :: Y.Value -> Value
    fromYaml yaml =
      case yaml of
        Y.Object o -> Map (fromYamlMap o)
        Y.Array a -> Array (map fromYaml (Vector.toList a))
        Y.String s -> Str s
        Y.Number n -> Num n
        Y.Bool b -> Bool b
        Y.Null -> Empty

    fromYamlMap :: Aeson.KeyMap.KeyMap Y.Value -> SymbolMap
    fromYamlMap a = Map.fromList (map f (Aeson.KeyMap.toList a))
      where
        f (k, v) = (Aeson.Key.toText k, fromYaml v)
