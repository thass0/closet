module EvalDoc (
  Env,
  empty,
  eval,
  Value (..),
  Symbol,
) where

import qualified Data.HashMap.Lazy as Map
import qualified Data.Yaml as Y
import Data.Text (Text)
import Data.Scientific
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Vector as Vector (toList)

import ParseDoc (Doc (..), FrontMatter, Block (..))

type Env = SymMap

type Symbol = Text

data Value
  = Str Text
  | Num Scientific
  | List [Value]
  | Map SymMap
  | Bool Bool
  | Null
  deriving (Show, Eq)

type SymMap = Map.HashMap Symbol Value

empty :: Env
empty = Map.empty

eval :: Env -> Doc -> (Env, Text)
eval env' doc =
  let env = env' <> fromFrontMatter (docFrontMatter doc)
  in foldl evalBlock (env, "") (docBlocks doc)
  where
    evalBlock :: (Env, Text) -> Block -> (Env, Text)
    evalBlock (e, t) (Cont c) = (e, t <> c)
    evalBlock s (Tag _) = s

fromFrontMatter :: FrontMatter -> Env
fromFrontMatter = fromYamlMap
  where
    fromYaml :: Y.Value -> Value
    fromYaml yaml = 
      case yaml of
        Y.Object o -> Map (fromYamlMap o)
        Y.Array a -> List (map fromYaml (Vector.toList a))
        Y.String s -> Str s
        Y.Number n -> Num n
        Y.Bool b -> Bool b
        Y.Null -> Null

    fromYamlMap :: (Aeson.KeyMap.KeyMap Y.Value) -> SymMap
    fromYamlMap a = Map.fromList (map (\(k, v) -> (Aeson.Key.toText k, fromYaml v)) (Aeson.KeyMap.toList a))