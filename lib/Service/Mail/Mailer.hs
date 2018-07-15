module Service.Mail.Mailer
  ( sendRegistrationConfirmationMail
  , sendRegistrationCreatedAnalyticsMail
  , sendResetPasswordMail
  ) where

import Control.Lens ((^.))
import Control.Monad.Reader (asks, liftIO)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.UUID as U
import qualified Network.Mail.SMTP as SMTP

import LensesConfig
import Model.Context.AppContext
import Model.User.User

createEmail :: Email -> T.Text -> TL.Text -> AppContextM ()
createEmail to subject body = do
  dswConfig <- asks _appContextConfig
  let mailConfig = dswConfig ^. mail
      addrFrom = SMTP.Address (Just . T.pack $ mailConfig ^. name) (T.pack $ mailConfig ^. email)
      cc = []
      bcc = []
      emailBody = SMTP.htmlPart body
      addrTo = [SMTP.Address Nothing (T.pack to)]
      mailHost = mailConfig ^. host
      mailUsername = mailConfig ^. username
      mailPassword = mailConfig ^. password
      mailMessage = SMTP.simpleMail addrFrom addrTo cc bcc subject [emailBody]
  if mailConfig ^. enabled
    then liftIO $ SMTP.sendMailWithLogin mailHost mailUsername mailPassword mailMessage
    else return ()

sendRegistrationConfirmationMail :: Email -> U.UUID -> String -> AppContextM ()
sendRegistrationConfirmationMail email userId hash = do
  dswConfig <- asks _appContextConfig
  let clientAddress = dswConfig ^. clientConfig . address
      clientLink = clientAddress ++ "/signup-confirmation/" ++ U.toString userId ++ "/" ++ hash
      link = "<a href=\"" ++ clientLink ++ "\">here</a>"
      subject = "Confirmation Email"
      body = TL.pack $ "Hi! For account activation you have to click " ++ link ++ "! Elixir DSW Wizard Team"
  createEmail email subject body

sendRegistrationCreatedAnalyticsMail :: String -> String -> Email -> AppContextM ()
sendRegistrationCreatedAnalyticsMail uName uSurname uEmail = do
  dswConfig <- asks _appContextConfig
  let analyticsAddress = dswConfig ^. analytics . email
      subject = "DSW Wizard: New user"
      body =
        TL.pack $ "Hi! We have a new user (" ++ uName ++ " " ++ uSurname ++ ", " ++ uEmail ++
        ") in our Wizard! Elixir DSW Wizard Team"
  createEmail analyticsAddress subject body

sendResetPasswordMail :: Email -> U.UUID -> String -> AppContextM ()
sendResetPasswordMail email userId hash = do
  dswConfig <- asks _appContextConfig
  let clientAddress = dswConfig ^. clientConfig . address
      clientLink = clientAddress ++ "/forgotten-password/" ++ U.toString userId ++ "/" ++ hash
      link = "<a href=\"" ++ clientLink ++ "\">here</a>"
      subject = "Reset Password"
      body = TL.pack $ "Hi! You can set up a new password " ++ link ++ "! Elixir DSW Wizard Team"
  createEmail email subject body
