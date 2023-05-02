module Registry.Service.Mail.Mailer (
  sendRegistrationConfirmationMail,
  sendRegistrationCreatedAnalyticsMail,
  sendResetTokenMail,
) where

import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (ToJSON)
import Data.Time
import qualified Data.UUID as U

import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Database.DAO.Common
import Registry.Database.DAO.PersistentCommand.PersistentCommandDAO
import Registry.Model.Config.ServerConfig
import Registry.Model.Context.AppContext
import Registry.Model.PersistentCommand.Mail.SendRegistrationConfirmationMailCommand
import Registry.Model.PersistentCommand.Mail.SendRegistrationCreatedAnalyticsMailCommand
import Registry.Model.PersistentCommand.Mail.SendResetTokenMailCommand
import Registry.Service.PersistentCommand.PersistentCommandMapper
import Shared.Common.Model.Config.ServerConfig
import Shared.Common.Util.JSON
import Shared.Common.Util.Uuid

sendRegistrationConfirmationMail :: OrganizationDTO -> String -> Maybe String -> AppContextM ()
sendRegistrationConfirmationMail org hash mCallbackUrl = do
  serverConfig <- asks serverConfig
  let clientAddress = serverConfig.general.clientUrl
  runInTransaction $ do
    let body =
          SendRegistrationConfirmationMailCommand
            { email = org.email
            , organizationId = org.organizationId
            , organizationName = org.name
            , organizationEmail = org.email
            , hash = hash
            , clientUrl = clientAddress
            , callbackUrl = mCallbackUrl
            }
    sendEmail "sendRegistrationConfirmationMail" body org.organizationId

sendRegistrationCreatedAnalyticsMail :: OrganizationDTO -> AppContextM ()
sendRegistrationCreatedAnalyticsMail org =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    let clientAddress = serverConfig.general.clientUrl
    let body =
          SendRegistrationCreatedAnalyticsMailCommand
            { email = serverConfig.analytics.email
            , organizationId = org.organizationId
            , organizationName = org.name
            , organizationEmail = org.email
            , clientUrl = clientAddress
            }
    sendEmail "sendRegistrationCreatedAnalyticsMail" body org.organizationId

sendResetTokenMail :: OrganizationDTO -> String -> AppContextM ()
sendResetTokenMail org hash =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    let clientAddress = serverConfig.general.clientUrl
    let body =
          SendResetTokenMailCommand
            { email = org.email
            , organizationId = org.organizationId
            , organizationName = org.name
            , organizationEmail = org.email
            , hash = hash
            , clientUrl = clientAddress
            }
    sendEmail "sendResetTokenMail" body org.organizationId

-- --------------------------------
-- PRIVATE
-- --------------------------------
sendEmail :: ToJSON dto => String -> dto -> String -> AppContextM ()
sendEmail function dto createdBy = do
  runInTransaction $ do
    pUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let body = encodeJsonToString dto
    let command = toPersistentCommand pUuid "mailer" function body 10 False U.nil createdBy now
    insertPersistentCommand command
    return ()
