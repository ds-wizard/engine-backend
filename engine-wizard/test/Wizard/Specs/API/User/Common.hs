module Wizard.Specs.API.User.Common where

import Data.Either (isLeft, isRight)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Database.DAO.User.UserDAO
import Wizard.Database.DAO.User.UserTokenDAO
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Model.App.App
import Wizard.Model.User.User
import Wizard.Service.User.UserUtil

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfUserInDB appContext user = do
  let uUuid = U.toString user.uuid
  eUser <- runInContextIO (findUserById uUuid) appContext
  liftIO $ isRight eUser `shouldBe` True
  let (Right userFromDB) = eUser
  compareUserDtos userFromDB user

assertPasswordOfUserInDB appContext user password = do
  let uUuid = U.toString user.uuid
  eUser <- runInContextIO (findUserById uUuid) appContext
  liftIO $ isRight eUser `shouldBe` True
  let (Right userFromDB) = eUser
  let isSame = verifyPassword password userFromDB.passwordHash
  liftIO $ isSame `shouldBe` True

assertAbsenceOfUserInDB appContext user = do
  let uUuid = U.toString user.uuid
  eUser <- runInContextIO (findUserById uUuid) appContext
  liftIO $ isLeft eUser `shouldBe` True
  let (Left error) = eUser
  liftIO $
    error
      `shouldBe` NotExistsError
        (_ERROR_DATABASE__ENTITY_NOT_FOUND "user_entity" [("app_uuid", U.toString defaultApp.uuid), ("uuid", uUuid)])

assertUserTokenInDB appContext user size = do
  eUserTokens <- runInContextIO (findUserTokensByUserUuid user.uuid) appContext
  liftIO $ isRight eUserTokens `shouldBe` True
  let (Right userTokens) = eUserTokens
  liftIO $ length userTokens `shouldBe` size

assertExistenceOfUserTokenInDB appContext user token = do
  eUserTokens <- runInContextIO (findUserTokensByUserUuid user.uuid) appContext
  liftIO $ isRight eUserTokens `shouldBe` True
  let (Right [userToken]) = eUserTokens
  liftIO $ userToken.value `shouldBe` token

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareUserDtos resDto expDto = liftIO $ resDto `shouldBe` expDto

compareUserCreateDtos resDto expDto userActive = do
  liftIO $ resDto.firstName `shouldBe` expDto.firstName
  liftIO $ resDto.lastName `shouldBe` expDto.lastName
  liftIO $ resDto.email `shouldBe` expDto.email
  liftIO $ resDto.affiliation `shouldBe` expDto.affiliation
  liftIO $ Just resDto.uRole `shouldBe` expDto.uRole
  liftIO $ resDto.active `shouldBe` userActive
