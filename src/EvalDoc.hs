{-# LANGUAGE InstanceSigs #-}

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
import Data.Maybe (isJust)
import Data.Scientific
import Data.Text (Text, intercalate, isInfixOf, pack, unpack)
import qualified Data.Vector as Vector (toList)
import qualified Data.Yaml as Y
import ParseDoc hiding (empty)

data Value
  = Str Text
  | Num Scientific
  | Array [Value]
  | Map SymMap
  | Bool Bool
  | Nil
  deriving (Show, Eq)

type Env = SymMap

type SymMap = Map.HashMap Symbol Value

type Symbol = [Text] -- Same as Var in ParseDoc.

empty :: Env
empty = Map.empty

instance Ord Value where
  (<=) :: Value -> Value -> Bool
  v1 <= v2 =
    case (v1, v2) of
      (Str s1, Str s2) -> s1 <= s2
      (Num n1, Num n2) -> n1 <= n2
      (Array a1, Array a2) -> length a1 <= length a2
      (Map m1, Map m2) -> length m1 <= length m2
      (Bool b1, Bool b2) -> not (b1 && not b2)
      (Nil, Nil) -> True
      (_, _) -> error "cannot compare different types"

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
        TagExpress expr ->
          let r = evalExpr e expr
           in (e, t <> literal r)
        _ -> error "not yet implemented"

    findBranch :: Env -> NE.NonEmpty (Expr, [Block]) -> Maybe [Block]
    findBranch e branches = snd <$> find (isTruthy . evalExpr e . fst) branches

evalExpr :: Env -> Expr -> Value
evalExpr env (Expr base filters) =
  let r = evalBase base
   in foldl (applyFilter env) r filters
  where
    -- Evaluate a base expression.
    evalBase :: BaseExpr -> Value
    evalBase expr =
      case expr of
        ImmVar var ->
          case Map.lookup var env of
            Just val -> val
            Nothing ->
              error $ "symbol lookup error: no variable called '" <> showSym var <> "' exists"
        ImmStrLit str -> Str str
        ImmNum num -> Num num
        ImmBool b -> Bool b
        ExprAnd a b ->
          if isTruthy (evalBase a)
            then Bool (isTruthy (evalBase b))
            else Bool False
        ExprOr a b ->
          if isTruthy (evalBase a)
            then Bool True
            else Bool (isTruthy (evalBase b))
        ExprEq a b -> Bool (evalBase a == evalBase b)
        ExprNeq a b -> Bool (evalBase a /= evalBase b)
        ExprGt a b -> Bool (evalBase a > evalBase b)
        ExprLt a b -> Bool (evalBase a < evalBase b)
        ExprGeq a b -> Bool (evalBase a >= evalBase b)
        ExprLeq a b -> Bool (evalBase a <= evalBase b)
        ExprContains a b ->
          case (evalBase a, evalBase b) of
            (Str super, Str sub) -> Bool (sub `isInfixOf` super)
            (Array strs, str) -> Bool (isJust (find (== str) strs))
            _ -> error "'contains' can only find substrings and strings in arrays"

    -- Apply a single filter expression to a value.
    applyFilter :: Env -> Value -> FilterExpr -> Value
    applyFilter _ v (FilterExpr ident args) =
      case (ident, args) of
        ("times", [s]) -> case (v, s) of
          (Num vn, ImmNum sn) -> Num (vn * sn)
          _ -> error "type error in 'times'"
        ("sum", [s]) -> case (v, s) of
          (Num vn, ImmNum sn) -> Num (vn + sn)
          _ -> error "type error in 'sum'"
        _ -> error "filter not yet implemented"

-- Evaluate the truthiness of a given value.
isTruthy :: Value -> Bool
isTruthy v =
  case v of
    Bool b -> b
    Nil -> False
    -- Empty string, empty array, and 0 are truthy, too.
    _ -> True

-- Print a symbol in the form it was originally parsed.
showSym :: Symbol -> String
showSym s = unpack $ intercalate "." s

-- Convert a value to a literal string that can be
-- added to the output document.
literal :: Value -> Text
literal v =
  case v of
    Str s -> s
    Num n -> pack (show n)
    -- TODO: Omit [] if the array has a depth of one.
    Array a -> "[" <> intercalate ", " (map literal a) <> "]"
    -- TODO: Omit {} if the map has a depth of one.
    Map m -> "{ " <> intercalate ", " (literalMap m) <> " }"
    Bool True -> "true"
    Bool False -> "false"
    Nil -> "nil"
  where
    literalSymbol :: Symbol -> Text
    literalSymbol = intercalate "."

    literalMap :: SymMap -> [Text]
    literalMap = map (\(k, v') -> literalSymbol k <> ": " <> literal v') . Map.toList

fromFrontMatter :: Aeson.KeyMap.KeyMap Y.Value -> SymMap
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
        Y.Null -> Nil

    fromYamlMap :: Aeson.KeyMap.KeyMap Y.Value -> SymMap
    fromYamlMap a = Map.fromList (map f (Aeson.KeyMap.toList a))
      where
        f (k, v) = ([Aeson.Key.toText k], fromYaml v)
