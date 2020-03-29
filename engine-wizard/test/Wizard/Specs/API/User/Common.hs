module Wizard.Specs.API.User.Common where

import Control.Lens ((^.))
import Data.Either (isLeft)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import LensesConfig
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Database.DAO.User.UserDAO

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertAbsenceOfUserInDB appContext user = do
  let uUuid = U.toString $ user ^. uuid
  eUser <- runInContextIO (findUserById uUuid) appContext
  liftIO $ isLeft eUser `shouldBe` True
  let (Left error) = eUser
  liftIO $ error `shouldBe` (NotExistsError $ _ERROR_DATABASE__ENTITY_NOT_FOUND "user" uUuid)
