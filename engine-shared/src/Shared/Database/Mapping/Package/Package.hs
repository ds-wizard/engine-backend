module Shared.Database.Mapping.Package.Package where

import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow

import Shared.Model.Package.Package

instance FromRow Package where
  fromRow = do
    _packagePId <- field
    _packageName <- field
    _packageOrganizationId <- field
    _packageKmId <- field
    _packageVersion <- field
    _packageMetamodelVersion <- field
    _packageDescription <- field
    _packageReadme <- field
    _packageLicense <- field
    _packagePreviousPackageId <- field
    _packageForkOfPackageId <- field
    _packageMergeCheckpointPackageId <- field
    _ <- field :: RowParser Value
    _packageCreatedAt <- field
    _packageAppUuid <- field
    return $ Package {..}
