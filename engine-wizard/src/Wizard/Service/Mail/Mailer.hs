module Wizard.Service.Mail.Mailer
  ( sendRegistrationConfirmationMail
  , sendRegistrationCreatedAnalyticsMail
  , sendResetPasswordMail
  ) where

import Control.Exception (SomeException, handle)
import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (emptyObject)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import Data.Either (rights)
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

import Shared.Localization.Messages.Internal
import Wizard.Api.Resource.User.UserDTO
import Wizard.Api.Resource.User.UserJM ()
import Wizard.Constant.Component
import Wizard.Constant.Mailer
import Wizard.LensesConfig
import Wizard.Localization.Messages.Internal
import Wizard.Model.Config.AppConfig
import Wizard.Model.Context.AppContext
import Wizard.Util.Logger
import Wizard.Util.Template (loadAndRender)

sendRegistrationConfirmationMail :: UserDTO -> String -> AppContextM (Either String ())
sendRegistrationConfirmationMail user hash = do
  appConfig <- asks _appContextApplicationConfig
  let clientAddress = appConfig ^. general . clientUrl
      activationLink = clientAddress ++ "/signup/" ++ U.toString (user ^. uuid) ++ "/" ++ hash
      mailName = appConfig ^. mail . name
      subject = TL.pack $ mailName ++ ": Confirmation Email"
      additionals = [("activationLink", Aeson.String $ T.pack activationLink)]
      context = makeMailContext appConfig user additionals
      to = [user ^. email]
  composeAndSendEmail to subject _MAIL_REGISTRATION_REGISTRATION_CONFIRMATION context

sendRegistrationCreatedAnalyticsMail :: UserDTO -> AppContextM (Either String ())
sendRegistrationCreatedAnalyticsMail user = do
  appConfig <- asks _appContextApplicationConfig
  let analyticsAddress = appConfig ^. analytics . email
      mailName = appConfig ^. mail . name
      subject = TL.pack $ mailName ++ ": New user"
      context = makeMailContext appConfig user []
      to = [analyticsAddress]
  composeAndSendEmail to subject _MAIL_REGISTRATION_CREATED_ANALYTICS context

sendResetPasswordMail :: UserDTO -> String -> AppContextM (Either String ())
sendResetPasswordMail user hash = do
  appConfig <- asks _appContextApplicationConfig
  let clientAddress = appConfig ^. general . clientUrl
      resetLink = clientAddress ++ "/forgotten-password/" ++ U.toString (user ^. uuid) ++ "/" ++ hash
      mailName = appConfig ^. mail . name
      subject = TL.pack $ mailName ++ ": Reset Password"
      additionals = [("resetLink", (Aeson.String $ T.pack resetLink))]
      context = makeMailContext appConfig user additionals
      to = [user ^. email]
  composeAndSendEmail to subject _MAIL_RESET_PASSWORD context

-- --------------------------------
-- PRIVATE
-- --------------------------------
type MailContext = HashMap T.Text Aeson.Value

composeAndSendEmail :: [String] -> TL.Text -> String -> MailContext -> AppContextM (Either String ())
composeAndSendEmail to subject mailName context = do
  mail <- composeMail to subject mailName context
  case mail of
    Right mailMessage -> sendEmail to mailMessage
    Left err -> return $ Left err

composeMail :: [String] -> TL.Text -> String -> MailContext -> AppContextM (Either String MIME.Mail)
composeMail to subject mailName context = do
  appConfig <- asks _appContextApplicationConfig
  let mailConfig = appConfig ^. mail
      addrFrom = MIME.Address (Just . T.pack $ mailConfig ^. name) (T.pack $ mailConfig ^. email)
      addrsTo = map (MIME.Address Nothing . T.pack) to
      emptyMail = MIME.Mail addrFrom addrsTo [] [] [("Subject", TL.toStrict subject)] []
      root = _MAIL_TEMPLATE_ROOT </> mailName
      commonRoot = _MAIL_TEMPLATE_ROOT </> _MAIL_TEMPLATE_COMMON_FOLDER
  plainTextPart <- makePlainTextPart (root </> _MAIL_TEMPLATE_PLAIN_NAME) context
  htmlPart <- makeHTMLPart (root </> _MAIL_TEMPLATE_HTML_NAME) context
  case (htmlPart, plainTextPart) of
    (Left _, Right _) -> logWarn $ msg _CMP_MAILER (_ERROR_SERVICE_MAIL__MISSING_HTML mailName)
    (Right _, Left _) -> logWarn $ msg _CMP_MAILER (_ERROR_SERVICE_MAIL__MISSING_PLAIN mailName)
    (Left _, Left _) -> logError $ msg _CMP_MAILER (_ERROR_SERVICE_MAIL__MISSING_HTML_PLAIN mailName)
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
      logWarn $ msg _CMP_MAILER (_ERROR_SERVICE_MAIL__FILE_LOAD_FAIL errMsg)
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

makeMailContext :: AppConfig -> UserDTO -> [(T.Text, Aeson.Value)] -> MailContext
makeMailContext appConfig user others =
  fromList $
  [ ("appTitle", Aeson.String . T.pack $ fromMaybe _MESSAGE_SERVICE_MAIL__APP_TITLE $ appConfig ^. client . appTitle)
  , ("mailName", Aeson.String . T.pack $ appConfig ^. mail . name)
  , ("clientAddress", Aeson.String . T.pack $ appConfig ^. general . clientUrl)
  , ("user", fromMaybe emptyObject . Aeson.decode . Aeson.encode $ user)
  ] ++
  others

makeConnection :: Integral i => Bool -> String -> i -> ((SMTP.SMTPConnection -> IO a) -> IO a)
makeConnection False host port = SMTP.doSMTPPort host (fromIntegral port)
makeConnection True host port = SMTPSSL.doSMTPSSLWithSettings host settings
  where
    settings = SMTPSSL.defaultSettingsSMTPSSL {SMTPSSL.sslPort = fromIntegral port}

sendEmail :: [String] -> MIME.Mail -> AppContextM (Either String ())
sendEmail [] mailMessage = return $ Left _ERROR_SERVICE_MAIL__TRIED_SEND_TO_NOONE
sendEmail to mailMessage = do
  appConfig <- asks _appContextApplicationConfig
  let mailConfig = appConfig ^. mail
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
  if mailConfig ^. enabled
    then do
      result <- liftIO $ handle basicHandler runMailer
      case result of
        Right recipients -> do
          logInfo $ msg _CMP_MAILER (_ERROR_SERVICE_MAIL__EMAIL_SENT_OK recipients)
          return $ Right ()
        Left excMsg -> do
          logError $ msg _CMP_MAILER (_ERROR_SERVICE_MAIL__EMAIL_SENT_FAIL excMsg)
          return $ Left excMsg
    else return $ Right ()
