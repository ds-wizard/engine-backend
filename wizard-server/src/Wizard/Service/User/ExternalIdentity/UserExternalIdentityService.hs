module Wizard.Service.User.ExternalIdentity.UserExternalIdentityService where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import qualified Data.UUID as U

import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import WizardLib.Public.Api.Resource.User.UserOpenIdIdentityDTO
import WizardLib.Public.Database.DAO.User.UserOpenIdIdentityDAO
import WizardLib.Public.Model.User.UserOpenIdIdentity
import WizardLib.Public.Service.User.UserOpenIdIdentityMapper

getUserIdentities :: AppContextM [UserOpenIdIdentityDTO]
getUserIdentities = do
  currentUser <- getCurrentUser
  identities <- findUserOpenIdIdentityListsByUserUuid (currentUser :: UserDTO).uuid
  return . fmap toDTO $ identities

deleteUserIdentity :: U.UUID -> AppContextM ()
deleteUserIdentity uuid =
  runInTransaction $ do
    currentUser <- getCurrentUser
    identities <- findUserOpenIdIdentitiesByUserUuid (currentUser :: UserDTO).uuid
    unless (any (\i -> (i :: UserOpenIdIdentity).uuid == uuid) identities) $
      throwError $
        NotExistsError (_ERROR_DATABASE__ENTITY_NOT_FOUND "user_openid_identity" [("uuid", U.toString uuid)])
    deleteUserOpenIdIdentityByUuid uuid
