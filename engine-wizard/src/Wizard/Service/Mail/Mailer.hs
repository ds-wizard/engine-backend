module Wizard.Service.Mail.Mailer
  ( sendRegistrationConfirmationMail
  , sendRegistrationCreatedAnalyticsMail
  , sendResetPasswordMail
  , sendQuestionnaireInvitationMail
  ) where

import Control.Exception (SomeException, handle)
import Control.Lens ((^.), (^..))
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (emptyObject)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import Data.Either (rights)
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap, fromList)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy as TL
import qualified Data.UUID as U
import qualified Network.HaskellNet.Auth as Auth
import qualified Network.HaskellNet.SMTP as SMTP
import qualified Network.HaskellNet.SMTP.SSL as SMTPSSL
import qualified Network.Mail.Mime as MIME
import qualified Network.Mime as MIME
import System.Directory (listDirectory)
import System.FilePath ((</>))

import LensesConfig
import Shared.Constant.Component
import Shared.Localization.Messages.Internal
import Shared.Model.Error.Error
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserJM ()
import Wizard.Constant.Mailer
import Wizard.Database.DAO.Common
import Wizard.Database.DAO.User.UserDAO
import Wizard.Localization.Messages.Internal
import Wizard.Model.Acl.Acl
import Wizard.Model.Config.AppConfig
import Wizard.Model.Config.ServerConfig
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireAcl
import Wizard.Service.App.AppService
import Wizard.Service.Config.AppConfigService
import Wizard.Service.User.UserMapper
import Wizard.Util.Logger
import Wizard.Util.Template (loadAndRender)

sendRegistrationConfirmationMail :: UserDTO -> String -> AppContextM ()
sendRegistrationConfirmationMail user hash =
  runInTransaction $ do
    serverConfig <- asks _appContextServerConfig
    appConfig <- getAppConfig
    clientUrl <- getAppClientUrl
    let activationLink = clientUrl ++ "/signup/" ++ U.toString (user ^. uuid) ++ "/" ++ hash
        mailName = serverConfig ^. mail . name
        subject = TL.pack $ mailName ++ ": " ++ _MAIL_SUBJECT_REGISTRATION_CONFIRMATION
        additionals = [("activationLink", Aeson.String $ T.pack activationLink)]
        context = makeMailContext serverConfig appConfig clientUrl user subject additionals
        to = [user ^. email]
    composeAndSendEmail to subject _MAIL_REGISTRATION_REGISTRATION_CONFIRMATION context

sendRegistrationCreatedAnalyticsMail :: UserDTO -> AppContextM ()
sendRegistrationCreatedAnalyticsMail user =
  runInTransaction $ do
    serverConfig <- asks _appContextServerConfig
    appConfig <- getAppConfig
    clientUrl <- getAppClientUrl
    let analyticsAddress = serverConfig ^. analytics . email
        mailName = serverConfig ^. mail . name
        subject = TL.pack $ mailName ++ ": " ++ _MAIL_SUBJECT_CREATED_ANALYTICS
        context = makeMailContext serverConfig appConfig clientUrl user subject []
        to = [analyticsAddress]
    composeAndSendEmail to subject _MAIL_REGISTRATION_CREATED_ANALYTICS context

sendResetPasswordMail :: UserDTO -> String -> AppContextM ()
sendResetPasswordMail user hash =
  runInTransaction $ do
    serverConfig <- asks _appContextServerConfig
    appConfig <- getAppConfig
    clientUrl <- getAppClientUrl
    let resetLink = clientUrl ++ "/forgotten-password/" ++ U.toString (user ^. uuid) ++ "/" ++ hash
        mailName = serverConfig ^. mail . name
        subject = TL.pack $ mailName ++ ": " ++ _MAIL_SUBJECT_RESET_PASSWORD
        additionals = [("resetLink", Aeson.String $ T.pack resetLink)]
        context = makeMailContext serverConfig appConfig clientUrl user subject additionals
        to = [user ^. email]
    composeAndSendEmail to subject _MAIL_RESET_PASSWORD context

sendQuestionnaireInvitationMail :: Questionnaire -> Questionnaire -> AppContextM ()
sendQuestionnaireInvitationMail oldQtn newQtn =
  runInTransaction $ traverse_ sendEmail (filter filterPermissions (newQtn ^. permissions))
  where
    filterPermissions :: QuestionnairePermRecord -> Bool
    filterPermissions perm = perm ^. member `notElem` (oldQtn ^.. permissions . traverse . member)
    sendEmail :: QuestionnairePermRecord -> AppContextM ()
    sendEmail permission =
      case permission ^. member of
        GroupMember {..} -> return ()
        UserMember {_userMemberUuid = userUuid} -> do
          currentUser <- getCurrentUser
          user <- findUserById (U.toString userUuid)
          serverConfig <- asks _appContextServerConfig
          appConfig <- getAppConfig
          clientUrl <- getAppClientUrl
          let projectLink = clientUrl ++ "/projects/" ++ U.toString (newQtn ^. uuid)
              mailName = serverConfig ^. mail . name
              subject = TL.pack $ mailName ++ ": " ++ _MAIL_SUBJECT_QUESTIONNAIRE_INVITATION
              additionals =
                [ ("projectLink", Aeson.String . T.pack $ projectLink)
                , ("projectName", Aeson.String . T.pack $ newQtn ^. name)
                , ("ownerFirstName", Aeson.String . T.pack $ currentUser ^. firstName)
                , ("ownerLastName", Aeson.String . T.pack $ currentUser ^. lastName)
                ]
              context = makeMailContext serverConfig appConfig clientUrl (toDTO user) subject additionals
              to = [user ^. email]
          composeAndSendEmail to subject _MAIL_QUESTIONNAIRE_INVITATION context

-- --------------------------------
-- PRIVATE
-- --------------------------------
type MailContext = HashMap T.Text Aeson.Value

composeAndSendEmail :: [String] -> TL.Text -> String -> MailContext -> AppContextM ()
composeAndSendEmail to subject mailName context = do
  mail <- composeMail to subject mailName context
  case mail of
    Right mailMessage -> sendEmail to mailMessage
    Left err -> throwError . GeneralServerError $ err

composeMail :: [String] -> TL.Text -> String -> MailContext -> AppContextM (Either String MIME.Mail)
composeMail to subject mailName context = do
  serverConfig <- asks _appContextServerConfig
  let mailConfig = serverConfig ^. mail
      addrFrom = MIME.Address (Just . T.pack $ mailConfig ^. name) (T.pack $ mailConfig ^. email)
      addrsTo = map (MIME.Address Nothing . T.pack) to
      emptyMail = MIME.Mail addrFrom addrsTo [] [] [("Subject", TL.toStrict subject)] []
      mailFolder = (serverConfig ^. general . templateFolder) ++ _MAIL_TEMPLATE_ROOT
      root = mailFolder </> mailName
      commonRoot = mailFolder </> _MAIL_TEMPLATE_COMMON_FOLDER
  plainTextPart <- makePlainTextPart (root </> _MAIL_TEMPLATE_PLAIN_NAME) context
  htmlPart <- makeHTMLPart (root </> _MAIL_TEMPLATE_HTML_NAME) context
  case (htmlPart, plainTextPart) of
    (Left _, Right _) -> logWarn _CMP_MAILER (_ERROR_SERVICE_MAIL__MISSING_HTML mailName)
    (Right _, Left _) -> logWarn _CMP_MAILER (_ERROR_SERVICE_MAIL__MISSING_PLAIN mailName)
    (Left _, Left _) -> logError _CMP_MAILER (_ERROR_SERVICE_MAIL__MISSING_HTML_PLAIN mailName)
    (_, _) -> return ()
  -- PLAIN alternative
  let mimeAlternative = rights [plainTextPart]
  -- HTML alternative with related inline images
  htmlAlternative <-
    case htmlPart of
      Right part -> do
        inlineImagesLocal <- makeInlineImages $ root </> _MAIL_TEMPLATE_IMAGES_FOLDER
        inlineImagesGlobal <- makeInlineImages $ commonRoot </> _MAIL_TEMPLATE_IMAGES_FOLDER
        return [MIME.relatedPart (part : inlineImagesLocal ++ inlineImagesGlobal)]
      _ -> return []
  let mailContent = mimeAlternative ++ htmlAlternative
      preparedMail = MIME.addPart mailContent emptyMail
  -- Finalize with attachments or fail
  case mailContent of
    [] -> return $ Left (_ERROR_SERVICE_MAIL__MISSING_HTML_PLAIN mailName)
    _ -> do
      attachmentsLocal <- makeAttachments $ root </> _MAIL_TEMPLATE_ATTACHMENTS_FOLDER
      attachmentsGlobal <- makeAttachments $ commonRoot </> _MAIL_TEMPLATE_ATTACHMENTS_FOLDER
      let attachments = attachmentsLocal ++ attachmentsGlobal
          finalMail = preparedMail {MIME.mailParts = MIME.mailParts preparedMail ++ attachments}
      return $ Right finalMail

basicHandler :: Monad m => SomeException -> m (Either String a)
basicHandler = return . Left . show

makePartsFromFiles :: FilePath -> (FilePath -> AppContextM (Maybe a)) -> AppContextM [a]
makePartsFromFiles root processFile = do
  eFiles <- liftIO $ handle basicHandler $ Right <$> listDirectory root
  case eFiles of
    Right files -> do
      parts <- mapM processFile files
      return $ catMaybes parts
    Left err -> return []

makePartFromFile :: FilePath -> (T.Text -> FilePath -> IO a) -> AppContextM (Maybe a)
makePartFromFile fullpath action = do
  result <-
    liftIO $ handle basicHandler $ do
      part <- action (E.decodeUtf8 $ MIME.defaultMimeLookup (T.pack fullpath)) fullpath
      return . Right $ part
  case result of
    Right part -> return . Just $ part
    Left errMsg -> do
      logWarn _CMP_MAILER (_ERROR_SERVICE_MAIL__FILE_LOAD_FAIL errMsg)
      return Nothing

makeInlineImages :: FilePath -> AppContextM [MIME.Part]
makeInlineImages root = makePartsFromFiles root (makeInlineImagePart root)

makeInlineImagePart :: FilePath -> FilePath -> AppContextM (Maybe MIME.Part)
makeInlineImagePart path filename = makePartFromFile (path </> filename) action
  where
    image ct fp = MIME.InlineImage ct (MIME.ImageFilePath fp) (T.pack filename)
    action contentType fullpath = MIME.addImage (image contentType fullpath)

makeAttachments :: FilePath -> AppContextM [MIME.Alternatives]
makeAttachments root = makePartsFromFiles root (makeAttachmentsPart root)

makeAttachmentsPart :: FilePath -> FilePath -> AppContextM (Maybe MIME.Alternatives)
makeAttachmentsPart path filename = makePartFromFile (path </> filename) action
  where
    action contentType fullpath = (: []) <$> MIME.filePart contentType fullpath

makeHTMLPart fn context =
  liftIO $ do
    template <- loadAndRender fn context
    return $ MIME.htmlPart . TL.fromStrict <$> template

makePlainTextPart fn context =
  liftIO $ do
    template <- loadAndRender fn context
    return $ MIME.plainPart . TL.fromStrict <$> template

makeMailContext :: ServerConfig -> AppConfig -> String -> UserDTO -> TL.Text -> [(T.Text, Aeson.Value)] -> MailContext
makeMailContext serverConfig appConfig clientUrl user subject others =
  fromList $
  [ ( "appTitle"
    , Aeson.String . T.pack $ fromMaybe _MESSAGE_SERVICE_MAIL__APP_TITLE $ appConfig ^. lookAndFeel . appTitle)
  , ("supportMail", Aeson.String . T.pack $ fromMaybe "" $ appConfig ^. privacyAndSupport . supportEmail)
  , ("mailName", Aeson.String . T.pack $ serverConfig ^. mail . name)
  , ("subject", Aeson.String . TL.toStrict $ subject)
  , ("clientAddress", Aeson.String . T.pack $ clientUrl)
  , ("user", fromMaybe emptyObject . Aeson.decode . Aeson.encode $ user)
  ] ++
  others

makeConnection :: Integral i => Bool -> String -> i -> ((SMTP.SMTPConnection -> IO a) -> IO a)
makeConnection False host port = SMTP.doSMTPPort host (fromIntegral port)
makeConnection True host port = SMTPSSL.doSMTPSSLWithSettings host settings
  where
    settings = SMTPSSL.defaultSettingsSMTPSSL {SMTPSSL.sslPort = fromIntegral port}

sendEmail :: [String] -> MIME.Mail -> AppContextM ()
sendEmail [] mailMessage = throwError . GeneralServerError $ _ERROR_SERVICE_MAIL__TRIED_SEND_TO_NOONE
sendEmail to mailMessage = do
  serverConfig <- asks _appContextServerConfig
  let mailConfig = serverConfig ^. mail
      from = mailConfig ^. email
      mailHost = mailConfig ^. host
      mailPort = mailConfig ^. port
      mailSSL = mailConfig ^. ssl
      mailAuthEnabled = mailConfig ^. authEnabled
      mailUsername = mailConfig ^. username
      mailPassword = mailConfig ^. password
      callback connection = do
        authSuccess <-
          if mailAuthEnabled
            then SMTP.authenticate Auth.LOGIN mailUsername mailPassword connection
            else return True
        renderedMail <- MIME.renderMail' mailMessage
        if authSuccess
          then do
            SMTP.sendMail from to (S.concat . B.toChunks $ renderedMail) connection
            return . Right $ to
          else return . Left $ _ERROR_SERVICE_MAIL__AUTH_ERROR_MESSAGE
      runMailer = makeConnection mailSSL mailHost mailPort callback
  when
    (mailConfig ^. enabled)
    (do result <- liftIO $ handle basicHandler runMailer
        case result of
          Right recipients -> do
            logInfo _CMP_MAILER (_ERROR_SERVICE_MAIL__EMAIL_SENT_OK recipients)
            return ()
          Left excMsg -> do
            logError _CMP_MAILER (_ERROR_SERVICE_MAIL__EMAIL_SENT_FAIL excMsg)
            throwError . GeneralServerError $ excMsg)
