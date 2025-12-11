module Wizard.Service.Project.Tag.ProjectTagService where

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Wizard.Database.DAO.Project.ProjectTagDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext

getProjectTagSuggestions :: Maybe String -> [String] -> Pageable -> [Sort] -> AppContextM (Page String)
getProjectTagSuggestions mQuery excludeTags pageable sort = do
  checkPermission _PRJ_PERM
  findProjectTagsPage mQuery excludeTags pageable sort
