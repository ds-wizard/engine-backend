module Wizard.Specs.API.User.Common where

import Control.Lens ((^.))
import Data.Either (isLeft, isRight)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Database.DAO.User.UserDAO
import Wizard.Service.Token.TokenService

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfUserInDB appContext user = do
  let uUuid = U.toString $ user ^. uuid
  eUser <- runInContextIO (findUserById uUuid) appContext
  liftIO $ isRight eUser `shouldBe` True
  let (Right userFromDB) = eUser
  compareUserDtos userFromDB user

assertPasswordOfUserInDB appContext user password = do
  let uUuid = U.toString $ user ^. uuid
  eUser <- runInContextIO (findUserById uUuid) appContext
  liftIO $ isRight eUser `shouldBe` True
  let (Right userFromDB) = eUser
  let isSame = verifyPassword password (userFromDB ^. passwordHash)
  liftIO $ isSame `shouldBe` True

assertAbsenceOfUserInDB appContext user = do
  let uUuid = U.toString $ user ^. uuid
  eUser <- runInContextIO (findUserById uUuid) appContext
  liftIO $ isLeft eUser `shouldBe` True
  let (Left error) = eUser
  liftIO $ error `shouldBe` (NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "user_entity" uUuid)

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareUserDtos resDto expDto = liftIO $ resDto `shouldBe` expDto

compareUserCreateDtos resDto expDto = do
  liftIO $ resDto ^. firstName `shouldBe` expDto ^. firstName
  liftIO $ resDto ^. lastName `shouldBe` expDto ^. lastName
  liftIO $ resDto ^. email `shouldBe` expDto ^. email
  liftIO $ resDto ^. affiliation `shouldBe` expDto ^. affiliation
  liftIO $ (Just $ resDto ^. role) `shouldBe` expDto ^. role
