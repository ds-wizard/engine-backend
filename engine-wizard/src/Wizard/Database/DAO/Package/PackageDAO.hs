module Wizard.Database.DAO.Package.PackageDAO where

import Shared.Database.Mapping.Package.Package ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Package.Package
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

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
    Nothing
    mOrganizationId
    mKmId
