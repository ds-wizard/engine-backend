module Wizard.Specs.Service.Token.TokenServiceSpec where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Time

import Test.Hspec

import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Localization.Messages.Internal
import Wizard.Model.Config.ServerConfig
import Wizard.Service.Token.TokenService
import Wizard.Util.Date

tokenServiceSpec =
  describe "Package Validation" $
  describe "verifyToken" $ do
    let secret = "01234567890123456789012345678901"
    let jwtVersion = 5
    let jwtExpirationInDays = 1
    let config =
          ServerConfigJwt {_serverConfigJwtVersion = jwtVersion, _serverConfigJwtExpiration = jwtExpirationInDays}
    let now = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    it "Successfull verification" $
        -- GIVEN:
     do
      let token = T.pack $ createToken userAlbert now config secret
        -- WHEN:
      let result = verifyToken token secret jwtVersion now
        -- THEN:
      result `shouldBe` Nothing
    it "Invalid signature" $
        -- GIVEN:
     do
      let token = T.pack $ createToken userAlbert now config secret ++ "invalid"
        -- WHEN:
      let result = verifyToken token secret jwtVersion now
        -- THEN:
      result `shouldBe` Just _ERROR_SERVICE_TOKEN__UNABLE_TO_DECODE_AND_VERIFY_TOKEN
    it "Obsolete JWT token version" $
        -- GIVEN:
     do
      let token = T.pack $ createToken userAlbert now config secret
        -- WHEN:
      let result = verifyToken token secret 6 now
        -- THEN:
      result `shouldBe` Just _ERROR_SERVICE_TOKEN__OBSOLETE_TOKEN_VERSION
    it "Token is expired" $ do
      let token = T.pack $ createToken userAlbert now config secret
      let timeDelta = realToFrac $ (jwtExpirationInDays + 1) * nominalDayInSeconds
      let tooFarFutureDate = addUTCTime timeDelta now
        -- WHEN:
      let result = verifyToken token secret jwtVersion tooFarFutureDate
        -- THEN:
      result `shouldBe` Just _ERROR_SERVICE_TOKEN__TOKEN_IS_EXPIRED
