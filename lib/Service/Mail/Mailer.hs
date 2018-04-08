module Service.Mail.Mailer
  ( sendRegistrationConfirmationMail
  , sendResetPasswordMail
  ) where

import Control.Lens ((^.))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.UUID as U
import qualified Network.Mail.SMTP as SMTP

import Common.Types
import LensesConfig
import Model.Config.DSWConfig

createEmail :: DSWConfig -> Email -> T.Text -> TL.Text -> IO ()
createEmail config to subject body =
  let mailConfig = config ^. mail
      addrFrom = SMTP.Address (Just . T.pack $ mailConfig ^. name) (T.pack $ mailConfig ^. email)
      cc = []
      bcc = []
      emailBody = SMTP.htmlPart body
      addrTo = [SMTP.Address Nothing (T.pack to)]
      mailHost = mailConfig ^. host
      mailUsername = mailConfig ^. username
      mailPassword = mailConfig ^. password
      mailMessage = SMTP.simpleMail addrFrom addrTo cc bcc subject [emailBody]
  in if mailConfig ^. enabled
       then SMTP.sendMailWithLogin mailHost mailUsername mailPassword mailMessage
       else return ()

sendRegistrationConfirmationMail :: DSWConfig -> Email -> U.UUID -> String -> IO ()
sendRegistrationConfirmationMail config email userId hash =
  let clientAddress = config ^. clientConfig . address
      clientLink = clientAddress ++ "/signup-confirmation/" ++ U.toString userId ++ "/" ++ hash
      link = "<a href=\"" ++ clientLink ++ "\">here</a>"
      subject = "Confirmation Email"
      body = TL.pack $ "Hi! Please click on this " ++ link ++ " to activate your account! Elixir DSW Wizard Team"
  in createEmail config email subject body

sendResetPasswordMail :: DSWConfig -> Email -> U.UUID -> String -> IO ()
sendResetPasswordMail config email userId hash =
  let clientAddress = config ^. clientConfig . address
      clientLink = clientAddress ++ "/forgotten-password/" ++ U.toString userId ++ "/" ++ hash
      link = "<a href=\"" ++ clientLink ++ "\">here</a>"
      subject = "Reset Password"
      body = TL.pack $ "Hi! You can set up a new password " ++ link ++ "! Elixir DSW Wizard Team"
  in createEmail config email subject body
