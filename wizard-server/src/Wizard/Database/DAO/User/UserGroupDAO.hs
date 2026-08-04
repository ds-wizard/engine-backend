module Wizard.Database.DAO.User.UserGroupDAO where

import Shared.Common.Model.Common.Page
import Shared.Common.Model.Common.Pageable
import Shared.Common.Model.Common.Sort
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.Mapping.User.UserGroupSuggestion ()
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Context.ContextLenses ()
import Wizard.Model.User.UserGroupSuggestion
import WizardLib.Public.Database.DAO.User.UserGroupDAO

entityName = "user_group"

pageLabel = "userGroups"

findUserGroupSuggestionsPage :: Maybe String -> Pageable -> [Sort] -> AppContextM (Page UserGroupSuggestion)
findUserGroupSuggestionsPage mQuery pageable sort = do
  currentUser <- getCurrentUser
  hasPermission <- hasPermission _USERS_MANAGE_ROLE_PERMISSION
  createFindUserGroupPage
    "ug.uuid, ug.name, ug.description, ug.private"
    currentUser.uuid
    hasPermission
    mQuery
    ""
    pageable
    sort
