module Wizard.Service.Mail.Mailer where

import Control.Monad.Reader (asks, liftIO)
import Data.Aeson (ToJSON)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as M
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Config.ServerConfig
import qualified Shared.Common.Model.PersistentCommand.Mail.MailCommand as MC
import Shared.Common.Util.JSON
import Shared.Common.Util.Uuid
import Shared.PersistentCommand.Database.DAO.PersistentCommand.PersistentCommandDAO
import Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandMapper
import Wizard.Api.Resource.User.UserDTO
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.User.UserDAO
import Wizard.Model.Acl.Acl
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.User.User
import Wizard.Service.Tenant.Config.ConfigService
import Wizard.Service.Tenant.TenantHelper
import WizardLib.Public.Model.User.UserToken

sendRegistrationConfirmationMail :: UserDTO -> String -> String -> AppContextM ()
sendRegistrationConfirmationMail user hash clientUrl =
  runInTransaction $ do
    tenantConfig <- getCurrentTenantConfig
    let body =
          MC.MailCommand
            { mode = "wizard"
            , template = "registrationConfirmation"
            , recipients = [user.email]
            , parameters =
                M.fromList
                  [ ("userUuid", MC.uuid user.uuid)
                  , ("userFirstName", MC.string user.firstName)
                  , ("userLastName", MC.string user.lastName)
                  , ("userEmail", MC.string user.email)
                  , ("hash", MC.string hash)
                  , ("clientUrl", MC.string clientUrl)
                  , ("appTitle", MC.maybeString tenantConfig.lookAndFeel.appTitle)
                  , ("supportEmail", MC.maybeString tenantConfig.privacyAndSupport.supportEmail)
                  ]
            }
    sendEmail body user.uuid

sendRegistrationCreatedAnalyticsMail :: UserDTO -> AppContextM ()
sendRegistrationCreatedAnalyticsMail user =
  runInTransaction $ do
    serverConfig <- asks serverConfig
    tenantConfig <- getCurrentTenantConfig
    clientUrl <- getClientUrl
    let body =
          MC.MailCommand
            { mode = "wizard"
            , template = "registrationCreatedAnalytics"
            , recipients = [serverConfig.analytics.email]
            , parameters =
                M.fromList
                  [ ("userUuid", MC.uuid user.uuid)
                  , ("userFirstName", MC.string user.firstName)
                  , ("userLastName", MC.string user.lastName)
                  , ("userEmail", MC.string user.email)
                  , ("clientUrl", MC.string clientUrl)
                  , ("appTitle", MC.maybeString tenantConfig.lookAndFeel.appTitle)
                  , ("supportEmail", MC.maybeString tenantConfig.privacyAndSupport.supportEmail)
                  ]
            }
    sendEmail body user.uuid

sendResetPasswordMail :: UserDTO -> String -> AppContextM ()
sendResetPasswordMail user hash =
  runInTransaction $ do
    tenantConfig <- getCurrentTenantConfig
    clientUrl <- getClientUrl
    let body =
          MC.MailCommand
            { mode = "wizard"
            , template = "resetPassword"
            , recipients = [user.email]
            , parameters =
                M.fromList
                  [ ("userUuid", MC.uuid user.uuid)
                  , ("userFirstName", MC.string user.firstName)
                  , ("userLastName", MC.string user.lastName)
                  , ("userEmail", MC.string user.email)
                  , ("hash", MC.string hash)
                  , ("clientUrl", MC.string clientUrl)
                  , ("appTitle", MC.maybeString tenantConfig.lookAndFeel.appTitle)
                  , ("supportEmail", MC.maybeString tenantConfig.privacyAndSupport.supportEmail)
                  ]
            }
    sendEmail body user.uuid

sendTwoFactorAuthMail :: UserDTO -> String -> AppContextM ()
sendTwoFactorAuthMail user code =
  runInTransaction $ do
    tenantConfig <- getCurrentTenantConfig
    clientUrl <- getClientUrl
    let body =
          MC.MailCommand
            { mode = "wizard"
            , template = "twoFactorAuth"
            , recipients = [user.email]
            , parameters =
                M.fromList
                  [ ("userUuid", MC.uuid user.uuid)
                  , ("userFirstName", MC.string user.firstName)
                  , ("userLastName", MC.string user.lastName)
                  , ("userEmail", MC.string user.email)
                  , ("code", MC.string code)
                  , ("clientUrl", MC.string clientUrl)
                  , ("appTitle", MC.maybeString tenantConfig.lookAndFeel.appTitle)
                  , ("supportEmail", MC.maybeString tenantConfig.privacyAndSupport.supportEmail)
                  ]
            }
    sendEmail body user.uuid

sendQuestionnaireInvitationMail :: Questionnaire -> Questionnaire -> AppContextM ()
sendQuestionnaireInvitationMail oldQtn newQtn =
  runInTransaction $ do
    tenantConfig <- getCurrentTenantConfig
    clientUrl <- getClientUrl
    currentUser <- getCurrentUser
    traverse_ (sendOneEmail tenantConfig clientUrl currentUser) (filter filterPermissions newQtn.permissions)
  where
    filterPermissions :: QuestionnairePermRecord -> Bool
    filterPermissions perm = perm.member `notElem` fmap (.member) oldQtn.permissions
    sendOneEmail :: TenantConfig -> String -> UserDTO -> QuestionnairePermRecord -> AppContextM ()
    sendOneEmail tenantConfig clientUrl currentUser permission =
      case permission.member of
        GroupMember {..} -> return ()
        UserMember {uuid = userUuid} -> do
          user <- findUserByUuid userUuid
          let body =
                MC.MailCommand
                  { mode = "wizard"
                  , template = "questionnaireInvitation"
                  , recipients = [user.email]
                  , parameters =
                      M.fromList
                        [ ("userUuid", MC.uuid user.uuid)
                        , ("clientUrl", MC.string clientUrl)
                        , ("appTitle", MC.maybeString tenantConfig.lookAndFeel.appTitle)
                        , ("supportEmail", MC.maybeString tenantConfig.privacyAndSupport.supportEmail)
                        , ("inviteeUuid", MC.uuid user.uuid)
                        , ("inviteeFirstName", MC.string user.firstName)
                        , ("inviteeLastName", MC.string user.lastName)
                        , ("inviteeEmail", MC.string user.email)
                        , ("questionnaireUuid", MC.uuid newQtn.uuid)
                        , ("questionnaireName", MC.string newQtn.name)
                        , ("ownerUuid", MC.uuid currentUser.uuid)
                        , ("ownerFirstName", MC.string currentUser.firstName)
                        , ("ownerLastName", MC.string currentUser.lastName)
                        , ("ownerEmail", MC.string currentUser.email)
                        ]
                  }
          sendEmail body currentUser.uuid

sendApiKeyCreatedMail :: UserDTO -> UserToken -> AppContextM ()
sendApiKeyCreatedMail user userToken =
  runInTransaction $ do
    tenantConfig <- getCurrentTenantConfig
    clientUrl <- getClientUrl
    let body =
          MC.MailCommand
            { mode = "wizard"
            , template = "apiKeyCreated"
            , recipients = [user.email]
            , parameters =
                M.fromList
                  [ ("userUuid", MC.uuid user.uuid)
                  , ("userFirstName", MC.string user.firstName)
                  , ("userLastName", MC.string user.lastName)
                  , ("userEmail", MC.string user.email)
                  , ("tokenName", MC.string userToken.name)
                  , ("tokenExpiresAt", MC.datetime userToken.expiresAt)
                  , ("clientUrl", MC.string clientUrl)
                  , ("appTitle", MC.maybeString tenantConfig.lookAndFeel.appTitle)
                  , ("supportEmail", MC.maybeString tenantConfig.privacyAndSupport.supportEmail)
                  ]
            }
    sendEmail body user.uuid

sendApiKeyExpirationMail :: UserDTO -> UserToken -> AppContextM ()
sendApiKeyExpirationMail user userToken =
  runInTransaction $ do
    tenantConfig <- getCurrentTenantConfig
    clientUrl <- getClientUrl
    let body =
          MC.MailCommand
            { mode = "wizard"
            , template = "apiKeyExpiration"
            , recipients = [user.email]
            , parameters =
                M.fromList
                  [ ("userUuid", MC.uuid user.uuid)
                  , ("userFirstName", MC.string user.firstName)
                  , ("userLastName", MC.string user.lastName)
                  , ("userEmail", MC.string user.email)
                  , ("tokenName", MC.string userToken.name)
                  , ("tokenExpiresAt", MC.datetime userToken.expiresAt)
                  , ("clientUrl", MC.string clientUrl)
                  , ("appTitle", MC.maybeString tenantConfig.lookAndFeel.appTitle)
                  , ("supportEmail", MC.maybeString tenantConfig.privacyAndSupport.supportEmail)
                  ]
            }
    sendEmail body user.uuid

-- --------------------------------
-- PRIVATE
-- --------------------------------
sendEmail :: ToJSON dto => dto -> U.UUID -> AppContextM ()
sendEmail dto createdBy = do
  runInTransaction $ do
    tenantUuid <- asks currentTenantUuid
    pUuid <- liftIO generateUuid
    now <- liftIO getCurrentTime
    let body = encodeJsonToString dto
    let command = toPersistentCommand pUuid "mailer" "sendMail" body 10 False Nothing tenantUuid (Just . U.toString $ createdBy) now
    insertPersistentCommand command
    return ()
