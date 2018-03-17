module Service.Mail.Mailer where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.Mail.SMTP

import Common.DSWConfig
import Common.Types

createEmail :: T.Text -> TL.Text -> AppConfigMail -> Email -> IO ()
createEmail subject body mailConfig to = do
  let addrFrom = Address (Just . T.pack $ mailConfig ^. acmName) (T.pack $ mailConfig ^. acmEmail)
  let cc = []
  let bcc = []
  let emailBody = plainTextPart body
  let addrTo = [Address Nothing (T.pack to)]
  let username = mailConfig ^. acmUsername
  let password = mailConfig ^. acmPassword
  let host = mailConfig ^. acmHost
  sendMailWithLogin host username password (simpleMail addrFrom addrTo cc bcc subject [emailBody])

--  let emailBody = htmlPart "<h1>HTML</h1>"
sendRegistrationConfirmationMail :: AppConfigMail -> Email -> IO ()
sendRegistrationConfirmationMail =
  createEmail
    "Activation Email"
    "Hello, please active your account by clicking on this link ....... Thanks! Elixir Team"
