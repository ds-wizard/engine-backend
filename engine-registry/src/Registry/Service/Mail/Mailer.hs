module Registry.Service.Mail.Mailer
  ( sendRegistrationConfirmationMail
  , sendRegistrationCreatedAnalyticsMail
  , sendResetTokenMail
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Registry.Api.Resource.Organization.OrganizationDTO
import Registry.Database.DAO.Common
import Registry.Database.DAO.PersistentCommand.PersistentCommandDAO
import Registry.Model.Context.AppContext
import Registry.Model.PersistentCommand.Mail.SendRegistrationConfirmationMailCommand
import Registry.Model.PersistentCommand.Mail.SendRegistrationCreatedAnalyticsMailCommand
import Registry.Model.PersistentCommand.Mail.SendResetTokenMailCommand
import Registry.Service.PersistentCommand.PersistentCommandMapper
import Shared.Util.Uuid

sendRegistrationConfirmationMail :: OrganizationDTO -> String -> Maybe String -> AppContextM ()
sendRegistrationConfirmationMail org hash mCallbackUrl = do
  serverConfig <- asks _appContextServerConfig
  let clientAddress = serverConfig ^. general . clientUrl
  runInTransaction $ do
    let body =
          SendRegistrationConfirmationMailCommand
            { _sendRegistrationConfirmationMailCommandEmail = org ^. email
            , _sendRegistrationConfirmationMailCommandOrganizationId = org ^. organizationId
            , _sendRegistrationConfirmationMailCommandHash = hash
            , _sendRegistrationConfirmationMailCommandClientUrl = clientAddress
            , _sendRegistrationConfirmationMailCommandCallbackUrl = mCallbackUrl
            }
    sendEmail "sendRegistrationConfirmationMail" (BSL.unpack . encode $ body) (org ^. organizationId)

sendRegistrationCreatedAnalyticsMail :: OrganizationDTO -> AppContextM ()
sendRegistrationCreatedAnalyticsMail org =
  runInTransaction $ do
    serverConfig <- asks _appContextServerConfig
    let clientAddress = serverConfig ^. general . clientUrl
    let body =
          SendRegistrationCreatedAnalyticsMailCommand
            { _sendRegistrationCreatedAnalyticsMailCommandEmail = serverConfig ^. analytics . email
            , _sendRegistrationCreatedAnalyticsMailCommandOrganizationId = org ^. organizationId
            , _sendRegistrationCreatedAnalyticsMailCommandClientUrl = clientAddress
            }
    sendEmail "sendRegistrationCreatedAnalyticsMail" (BSL.unpack . encode $ body) (org ^. organizationId)

sendResetTokenMail :: OrganizationDTO -> String -> AppContextM ()
sendResetTokenMail org hash =
  runInTransaction $ do
    serverConfig <- asks _appContextServerConfig
    let clientAddress = serverConfig ^. general . clientUrl
    let body =
          SendResetTokenMailCommand
            { _sendResetTokenMailCommandEmail = org ^. email
            , _sendResetTokenMailCommandOrganizationId = org ^. organizationId
            , _sendResetTokenMailCommandHash = hash
            , _sendResetTokenMailCommandClientUrl = clientAddress
            }
    sendEmail "sendResetTokenMail" (BSL.unpack . encode $ body) (org ^. organizationId)

-- --------------------------------
-- PRIVATE
-- --------------------------------
sendEmail :: String -> String -> String -> AppContextM ()
sendEmail function body createdBy = do
  runInTransaction $ do
    pUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let command = toPersistentCommand pUuid "Mailer" function body 10 False U.nil createdBy now
    insertPersistentCommand command
    return ()
