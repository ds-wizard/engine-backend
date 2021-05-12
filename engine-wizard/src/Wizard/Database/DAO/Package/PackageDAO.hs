module Wizard.Database.DAO.Package.PackageDAO where

import Data.String (fromString)
import Database.PostgreSQL.Simple

import Shared.Database.Mapping.Package.Package ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Package.Package
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Util.Logger

entityName = "package"

pageLabel = "packages"

findPackagesPage :: Maybe String -> Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page Package)
findPackagesPage mOrganizationId mKmId mQuery pageable sort =
  createFindEntitiesGroupByCoordinatePageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    "km_id"
    mQuery
    mOrganizationId
    mKmId

findVersionsForPackage :: String -> String -> AppContextM [String]
findVersionsForPackage orgId kmId = do
  let sql = "SELECT version FROM package WHERE organization_id = ? and km_id = ?"
  logInfo _CMP_DATABASE sql
  let action conn = query conn (fromString sql) [orgId, kmId]
  versions <- runDB action
  return . fmap fromOnly $ versions
