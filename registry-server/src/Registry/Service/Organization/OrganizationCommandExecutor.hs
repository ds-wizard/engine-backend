module Registry.Service.Organization.OrganizationCommandExecutor where

import Control.Monad.Except (throwError)
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as BSL

import Registry.Model.Context.AppContext
import Registry.Service.Organization.OrganizationService
import RegistryLib.Api.Resource.Organization.OrganizationCreateDTO
import RegistryLib.Api.Resource.Organization.OrganizationCreateJM ()
import Shared.Common.Model.Error.Error
import Shared.Common.Util.Logger
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand

cComponent = "organization"

execute :: PersistentCommand String -> AppContextM (PersistentCommandState, Maybe String)
execute command
  | command.function == cCreateOrganizationName = cCreateOrganization command
  | otherwise = throwError . GeneralServerError $ "Unknown command function: " <> command.function

cCreateOrganizationName = "createOrganization"

cCreateOrganization :: PersistentCommand String -> AppContextM (PersistentCommandState, Maybe String)
cCreateOrganization persistentCommand = do
  let eCommand = eitherDecode (BSL.pack persistentCommand.body) :: Either String OrganizationCreateDTO
  case eCommand of
    Right command -> do
      createOrganization command Nothing
      return (DonePersistentCommandState, Nothing)
    Left error -> return (ErrorPersistentCommandState, Just $ f' "Problem in deserialization of JSON: %s" [error])
