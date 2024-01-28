module DocModel (
  Ident,
  Symbol,
  showIdent,
) where

import qualified Data.List.NonEmpty as NE
import Data.Text (Text, intercalate, unpack)

type Ident = NE.NonEmpty Symbol

type Symbol = Text

showIdent :: Ident -> String
showIdent i = unpack $ intercalate "." (NE.toList i)
