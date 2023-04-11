module Wizard.Database.Migration.Production.Migration_0032_branchFix.Migration (
  definition,
) where

import Control.Monad.Logger
import Control.Monad.Reader (liftIO)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Migration.Entity
import Database.PostgreSQL.Simple

definition = (meta, migrate)

meta = MigrationMeta {mmNumber = 32, mmName = "Branch Fix", mmDescription = "Fix version, description, readme and license in branch"}

migrate :: Pool Connection -> LoggingT IO (Maybe Error)
migrate dbPool = do
  adjustVersionInBranch dbPool
  adjustDescriptionInBranch dbPool
  adjustReadmeInBranch dbPool
  adjustLicenseInBranch dbPool

adjustVersionInBranch dbPool = do
  let sql =
        "UPDATE branch \
        \SET version=previous_package.version \
        \FROM (SELECT inner_branch.uuid as branch_uuid, inner_package.version as version \
        \      FROM branch inner_branch \
        \      JOIN package inner_package ON inner_package.id = inner_branch.previous_package_id \
        \      WHERE inner_branch.version = '1.0.0' \
        \        AND (inner_branch.description = 'Fill description here' \
        \                 OR inner_branch.readme = 'Fill readme here' \
        \                 OR inner_branch.license = 'Fill license here') \
        \     ) as previous_package \
        \WHERE branch.uuid = previous_package.branch_uuid;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

adjustDescriptionInBranch dbPool = do
  let sql =
        "UPDATE branch \
        \SET description=previous_package.description \
        \FROM (SELECT inner_branch.uuid as branch_uuid, inner_package.description as description \
        \      FROM branch inner_branch \
        \      JOIN package inner_package ON inner_package.id = inner_branch.previous_package_id \
        \      WHERE inner_branch.description = 'Fill description here' \
        \     ) as previous_package \
        \WHERE branch.uuid = previous_package.branch_uuid;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

adjustReadmeInBranch dbPool = do
  let sql =
        "UPDATE branch \
        \SET readme=previous_package.readme \
        \FROM (SELECT inner_branch.uuid as branch_uuid, inner_package.readme as readme \
        \      FROM branch inner_branch \
        \      JOIN package inner_package ON inner_package.id = inner_branch.previous_package_id \
        \      WHERE inner_branch.readme = 'Fill readme here' \
        \     ) as previous_package \
        \WHERE branch.uuid = previous_package.branch_uuid;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing

adjustLicenseInBranch dbPool = do
  let sql =
        "UPDATE branch \
        \SET license=previous_package.license \
        \FROM (SELECT inner_branch.uuid as branch_uuid, inner_package.license as license \
        \      FROM branch inner_branch \
        \      JOIN package inner_package ON inner_package.id = inner_branch.previous_package_id \
        \      WHERE inner_branch.license = 'Fill license here' \
        \     ) as previous_package \
        \WHERE branch.uuid = previous_package.branch_uuid;"
  let action conn = execute_ conn sql
  liftIO $ withResource dbPool action
  return Nothing
