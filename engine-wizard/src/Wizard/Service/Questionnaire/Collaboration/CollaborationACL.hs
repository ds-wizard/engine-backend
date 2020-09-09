module Wizard.Service.Questionnaire.Collaboration.CollaborationACL where

import Control.Lens ((^.))
import Control.Monad.Except (throwError)
import Data.Maybe (isJust)
import Prelude hiding (log)

import LensesConfig
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.User.User
import Wizard.Model.Websocket.WebsocketRecord

getPermission visibility sharing mOwnerUuid mCurrentUserUuid mCurrentUserRole =
  let isOwner = mOwnerUuid == mCurrentUserUuid
      isLogged = isJust mCurrentUserUuid
      isAdmin = mCurrentUserRole == Just _USER_ROLE_ADMIN
   in check visibility sharing isOwner isLogged isAdmin
  where
    check _ _ _ _ True = EditEntityPerm
    check _ _ True True False = EditEntityPerm
    check _ AnyoneWithLinkEditQuestionnaire _ _ False = EditEntityPerm
    check VisibleEditQuestionnaire _ _ True False = EditEntityPerm
    check _ AnyoneWithLinkViewQuestionnaire _ _ False = ViewEntityPerm
    check VisibleViewQuestionnaire _ _ True False = ViewEntityPerm
    check _ _ _ _ _ = NoEntityPerm

checkViewPermission myself =
  if myself ^. entityPerm == EditEntityPerm || myself ^. entityPerm == ViewEntityPerm
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "View Questionnaire"

checkEditPermission myself =
  if myself ^. entityPerm == EditEntityPerm
    then return ()
    else throwError . ForbiddenError $ _ERROR_VALIDATION__FORBIDDEN "Edit Questionnaire"
