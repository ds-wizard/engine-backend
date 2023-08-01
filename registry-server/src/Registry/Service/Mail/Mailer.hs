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

import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Database.DAO.Common
import Registry.Model.Config.ServerConfig
import Registry.Model.Context.AppContext
import Shared.Common.Model.Config.ServerConfig
import qualified Shared.Common.Model.PersistentCommand.Mail.MailCommand as MC
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
            , recipients = [org.email]
            , parameters =
                M.fromList
                  [ ("organizationId", MC.string org.organizationId)
                  , ("organizationName", MC.string org.name)
                  , ("organizationEmail", MC.string org.email)
                  , ("hash", MC.string hash)
                  , ("clientUrl", MC.string clientAddress)
                  , ("callbackUrl", MC.maybeString mCallbackUrl)
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
            , recipients = [serverConfig.analytics.email]
            , parameters =
                M.fromList
                  [ ("organizationId", MC.string org.organizationId)
                  , ("organizationName", MC.string org.name)
                  , ("organizationEmail", MC.string org.email)
                  , ("clientUrl", MC.string clientAddress)
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
            , recipients = [org.email]
            , parameters =
                M.fromList
                  [ ("organizationId", MC.string org.organizationId)
                  , ("organizationName", MC.string org.name)
                  , ("organizationEmail", MC.string org.email)
                  , ("hash", MC.string hash)
                  , ("clientUrl", MC.string clientAddress)
                  ]
            }
    sendEmail body org.organizationId

-- --------------------------------
-- PRIVATE
-- --------------------------------
sendEmail :: ToJSON dto => dto -> String -> AppContextM ()
sendEmail dto createdBy = do
  runInTransaction $ do
    pUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let body = encodeJsonToString dto
    let command = toPersistentCommand pUuid "mailer" "sendMail" body 10 False Nothing U.nil (Just createdBy) now
    insertPersistentCommand command
    return ()
