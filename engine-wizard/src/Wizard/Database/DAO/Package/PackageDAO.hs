module Wizard.Database.DAO.Package.PackageDAO where

import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Package.PackageList ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Package.PackageList

entityName = "package"

pageLabel = "packages"

findPackagesPage :: Maybe String -> Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page PackageList)
findPackagesPage mOrganizationId mKmId mQuery pageable sort =
  createFindEntitiesGroupByCoordinatePageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "id, package.name, package.organization_id, package.km_id, version, description, registry_package.remote_version, registry_organization.name as org_name, registry_organization.logo as org_logo, package.created_at"
    "km_id"
    mQuery
    Nothing
    mOrganizationId
    mKmId
