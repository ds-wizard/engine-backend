module Wizard.Service.Project.User.ProjectUserService where

import qualified Data.UUID as U

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectUserDAO
import Wizard.Model.Context.AppContext
import Wizard.Model.Project.Project
import Wizard.Service.Project.ProjectAcl
import Wizard.Service.User.UserMapper
import Wizard.Service.User.UserService
import WizardLib.Public.Model.User.UserSuggestion

getProjectUserSuggestionsPage :: U.UUID -> Maybe String -> Maybe Bool -> Pageable -> [Sort] -> AppContextM (Page UserSuggestion)
getProjectUserSuggestionsPage projectUuid mQuery mEditor pageable sort = do
  project <- findProjectByUuid projectUuid
  checkCommentPermissionToProject project.visibility project.sharing project.permissions
  if project.visibility == VisibleCommentProjectVisibility || project.visibility == VisibleEditProjectVisibility || project.sharing == AnyoneWithLinkCommentProjectSharing || project.sharing == AnyoneWithLinkEditProjectSharing
    then getUserSuggestionsPage mQuery Nothing Nothing pageable sort
    else do
      let perm =
            case mEditor of
              Just True -> "EDIT"
              _ -> "COMMENT"
      suggestionPage <- findProjectUserSuggestionsPage projectUuid perm mQuery pageable sort
      return . fmap toSuggestion $ suggestionPage
