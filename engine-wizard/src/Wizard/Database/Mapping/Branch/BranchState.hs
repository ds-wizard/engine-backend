module Wizard.Database.Mapping.Branch.BranchState where

import qualified Data.ByteString.Char8 as BS
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField

import Wizard.Model.Branch.BranchState

instance FromField BranchState where
  fromField f dat =
    case fmap BS.unpack dat of
      Just "BSDefault" -> return BSDefault
      Just "BSEdited" -> return BSEdited
      Just "BSOutdated" -> return BSOutdated
      Just "BSMigrating" -> return BSMigrating
      Just "BSMigrated" -> return BSMigrated
      _ -> returnError ConversionFailed f "Unsupported type"
