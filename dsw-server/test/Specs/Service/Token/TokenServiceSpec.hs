module Specs.Service.Token.TokenServiceSpec where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Time

import Test.Hspec

import Database.Migration.Development.User.Data.Users
import Localization
import Service.Token.TokenService
import Util.Date

tokenServiceSpec =
  describe "Package Validation" $ do
    describe "verifyToken" $ do
      let jwtSecret = "my-secret"
      let jwtVersion = 5
      let jwtExpirationInDays = 1
      let now = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
      it "Successfull verification" $
        -- GIVEN:
       do
        let token = T.pack $ createToken userAlbert now jwtSecret jwtVersion jwtExpirationInDays
        -- WHEN:
        let result = verifyToken token jwtSecret jwtVersion now
        -- THEN:
        result `shouldBe` Nothing
      it "Invalid signature" $
        -- GIVEN:
       do
        let token = T.pack $ (createToken userAlbert now jwtSecret jwtVersion jwtExpirationInDays) ++ "invalid"
        -- WHEN:
        let result = (verifyToken token jwtSecret jwtVersion now)
        -- THEN:
        result `shouldBe` Just _ERROR_SERVICE_TOKEN__UNABLE_TO_DECODE_AND_VERIFY_TOKEN
      it "Obsolete JWT token version" $
        -- GIVEN:
       do
        let token = T.pack $ createToken userAlbert now jwtSecret jwtVersion jwtExpirationInDays
        -- WHEN:
        let result = verifyToken token jwtSecret 6 now
        -- THEN:
        result `shouldBe` Just _ERROR_SERVICE_TOKEN__OBSOLETE_TOKEN_VERSION
      it "Token is expired" $ do
        let token = T.pack $ createToken userAlbert now jwtSecret jwtVersion jwtExpirationInDays
        let timeDelta = realToFrac $ (jwtExpirationInDays + 1) * nominalDayInSeconds
        let tooFarFutureDate = addUTCTime timeDelta now
        -- WHEN:
        let result = verifyToken token jwtSecret jwtVersion tooFarFutureDate
        -- THEN:
        result `shouldBe` Just _ERROR_SERVICE_TOKEN__TOKEN_IS_EXPIRED
