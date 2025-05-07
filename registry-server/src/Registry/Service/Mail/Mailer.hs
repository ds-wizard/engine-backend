module Registry.Service.Mail.Mailer (
  sendRegistrationConfirmationMail,
  sendRegistrationCreatedAnalyticsMail,
  sendResetTokenMail,
) where

import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (ToJSON)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Registry.Database.DAO.Common
import Registry.Model.Config.ServerConfig
import Registry.Model.Context.AppContext
import RegistryLib.Api.Resource.Organization.OrganizationDTO
import Shared.Common.Model.Config.ServerConfig
import qualified Shared.Common.Model.PersistentCommand.Mail.MailCommand as MC
import qualified Shared.Common.Util.Aeson as A
import Shared.Common.Util.JSON
import Shared.Common.Util.Uuid
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandMapper

sendRegistrationConfirmationMail :: OrganizationDTO -> String -> Maybe String -> AppContextM ()
sendRegistrationConfirmationMail org hash mCallbackUrl = do
  serverConfig <- asks serverConfig
  let clientAddress = serverConfig.general.clientUrl
  runInTransaction $ do
    let body =
          MC.MailCommand
            { mode = "registry"
            , template = "registrationConfirmation"
            , recipients = [MC.MailRecipient {uuid = Nothing, email = org.email}]
            , parameters =
                M.fromList
                  [ ("organizationId", A.string org.organizationId)
                  , ("organizationName", A.string org.name)
                  , ("organizationEmail", A.string org.email)
                  , ("hash", A.string hash)
                  , ("clientUrl", A.string clientAddress)
                  , ("callbackUrl", A.maybeString mCallbackUrl)
                  ]
            }
    sendEmail body org.organizationId

sendRegistrationCreatedAnalyticsMail :: OrganizationDTO -> AppContextM ()
sendRegistrationCreatedAnalyticsMail org =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    let clientAddress = serverConfig.general.clientUrl
    let body =
          MC.MailCommand
            { mode = "registry"
            , template = "registrationCreatedAnalytics"
            , recipients = [MC.MailRecipient {uuid = Nothing, email = serverConfig.analyticalMails.email}]
            , parameters =
                M.fromList
                  [ ("organizationId", A.string org.organizationId)
                  , ("organizationName", A.string org.name)
                  , ("organizationEmail", A.string org.email)
                  , ("clientUrl", A.string clientAddress)
                  ]
            }
    sendEmail body org.organizationId

sendResetTokenMail :: OrganizationDTO -> String -> AppContextM ()
sendResetTokenMail org hash =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    let clientAddress = serverConfig.general.clientUrl
    let body =
          MC.MailCommand
            { mode = "registry"
            , template = "resetToken"
            , recipients = [MC.MailRecipient {uuid = Nothing, email = org.email}]
            , parameters =
                M.fromList
                  [ ("organizationId", A.string org.organizationId)
                  , ("organizationName", A.string org.name)
                  , ("organizationEmail", A.string org.email)
                  , ("hash", A.string hash)
                  , ("clientUrl", A.string clientAddress)
                  ]
            }
    sendEmail body org.organizationId

-- --------------------------------
-- PRIVATE
-- --------------------------------
sendEmail :: ToJSON dto => dto -> String -> AppContextM ()
sendEmail dto createdBy = do
  runInTransaction $ do
    uuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let body = encodeJsonToString dto
    let command = toPersistentCommand uuid "mailer" "sendMail" body 10 False Nothing U.nil (Just createdBy) now
    insertPersistentCommand command
    return ()
