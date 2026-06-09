module Wizard.Service.OpenId.Client.Definition.OpenIdClientDefinitionCommandExecutor where

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (liftIO)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.ContextLenses ()
import Wizard.Service.OpenId.Client.Definition.OpenIdClientDefinitionMapper
import WizardLib.Public.Database.DAO.OpenId.OpenIdClientDefinitionDAO
import WizardLib.Public.Model.PersistentCommand.OpenId.CreateOrUpdateOpenIdClientDefinitionCommand

cComponent = "openid_client_definition"

execute :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cCreateOrUpdateName = cCreateOrUpdate command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cCreateOrUpdateName = "createOrUpdate"

cCreateOrUpdate :: PersistentCommand U.UUID -> AppContextM (PersistentCommandState, Maybe String)
cCreateOrUpdate persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String CreateOrUpdateOpenIdClientDefinitionCommand
  case eCommand of
    Right command -> do
      mOpenIdClient <- findOpenIdClientDefinitionByUuidAndTenantUuid' persistentCommand.uuid persistentCommand.tenantUuid
      now <- liftIO getCurrentTime
      case mOpenIdClient of
        Just openIdClient -> do
          let openIdClientUpdated = fromUpdate openIdClient command now
          void $ updateOpenIdClientDefinition openIdClientUpdated
        Nothing -> do
          let openIdClient = fromCreate command persistentCommand.tenantUuid now
          void $ insertOpenIdClientDefinition openIdClient
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
