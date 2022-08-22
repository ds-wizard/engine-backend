module Wizard.Database.DAO.Template.TemplateDAO where

import Shared.Database.Mapping.Template.Template ()
import Shared.Model.Common.Page
import Shared.Model.Common.Pageable
import Shared.Model.Common.Sort
import Shared.Model.Template.Template
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()

entityName = "template"

pageLabel = "templates"

findTemplatesPage :: Maybe String -> Maybe String -> Maybe String -> Pageable -> [Sort] -> AppContextM (Page Template)
findTemplatesPage mOrganizationId mTemplateId mQuery pageable sort =
  createFindEntitiesGroupByCoordinatePageableQuerySortFn
    entityName
    pageLabel
    pageable
    sort
    "*"
    "template_id"
    mQuery
    Nothing
    mOrganizationId
    mTemplateId
