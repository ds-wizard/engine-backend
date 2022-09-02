module Wizard.Database.DAO.Template.TemplateDAO where

import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Wizard.Database.DAO.Common
import Wizard.Database.Mapping.Template.TemplateList ()
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.Template.TemplateList

entityName = "template"

pageLabel = "templates"

findTemplatesPage ::
     Maybe String -> Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page TemplateList)
findTemplatesPage mOrganizationId mTemplateId mQuery pageable sort =
  createFindEntitiesGroupByCoordinatePageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "template.*, registry_template.remote_version, registry_organization.name as org_name, registry_organization.logo as org_logo"
    "template_id"
    mQuery
    Nothing
    mOrganizationId
    mTemplateId
