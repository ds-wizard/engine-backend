module Database.DAO.Common where

import Data.Bson
import qualified Data.Text.Lazy as LT
import qualified Data.UUID as U

instance Val LT.Text where
  val = String . LT.toStrict
  cast' (String x) = Just . LT.fromStrict $ x
  cast' (Sym (Symbol x)) = Just . LT.fromStrict $ x
  cast' _ = Nothing

instance Val U.UUID where
  val = String . U.toText
  cast' (String x) = U.fromText x
  cast' (Sym (Symbol x)) = U.fromText x
  cast' _ = Nothing
