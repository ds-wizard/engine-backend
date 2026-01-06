module Wizard.Specs.API.ProjectAction.Common where

import Data.Either (isRight)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Wizard.Database.DAO.Project.ProjectActionDAO
import Wizard.Model.Project.Action.ProjectAction

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfProjectActionInDB appContext action = do
  eProjectAction <- runInContextIO (findProjectActionById action.paId) appContext
  liftIO $ isRight eProjectAction `shouldBe` True
  let (Right actionFromDB) = eProjectAction
  compareProjectActionDtos actionFromDB action

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareProjectActionDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.enabled `shouldBe` expDto.enabled
