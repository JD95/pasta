module Parser.Lexer where

import Data.Char
import Data.Either
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Numeric.Natural
import Text.Read (readMaybe)

data Token
  = TNat Natural
  | TInt Int
  | TDbl Double
  deriving (Eq, Show)

toMaybe :: Either e a -> Maybe a
toMaybe = either (const Nothing) Just

natNum :: Text -> Maybe Token
natNum = fmap (TNat . fst) . toMaybe . Text.decimal

intNum :: Text -> Maybe Token
intNum = fmap (TInt . fst) . toMaybe . Text.signed Text.decimal

dblNum :: Text -> Maybe Token
dblNum = fmap (TDbl . fst) . toMaybe . Text.signed Text.double
