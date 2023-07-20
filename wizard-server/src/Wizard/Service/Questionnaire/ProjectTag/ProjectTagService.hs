module Wizard.Service.Questionnaire.ProjectTag.ProjectTagService where

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Wizard.Database.DAO.Questionnaire.QuestionnaireProjectTagDAO
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext

getProjectTagSuggestions :: Maybe String -> [String] -> Pageable -> [Sort] -> AppContextM (Page String)
getProjectTagSuggestions mQuery excludeTags pageable sort = do
  checkPermission _QTN_PERM
  findQuestionnaireProjectTagsPage mQuery excludeTags pageable sort
