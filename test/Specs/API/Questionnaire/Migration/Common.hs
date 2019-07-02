module Specs.API.Questionnaire.Migration.Common where

import Control.Lens ((^.))
import Data.Either (isRight)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Api.Resource.Error.ErrorDTO ()
import Database.DAO.Migration.Questionnaire.MigratorDAO
import LensesConfig

import Specs.API.Questionnaire.Common
import Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfMigrationStateInDB appContext entity = do
  eEntityFromDb <-
    runInContextIO (findMigratorStateByOldQuestionnaireId (U.toString $ entity ^. oldQuestionnaireUuid)) appContext
  liftIO $ (isRight eEntityFromDb) `shouldBe` True
  let (Right entityFromDb) = eEntityFromDb
  compareQtnMigrators entityFromDb entity

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareQtnMigrators resDto expDto = do
  liftIO $ resDto ^. oldQuestionnaireUuid `shouldBe` expDto ^. oldQuestionnaireUuid
  liftIO $ resDto ^. newQuestionnaireUuid `shouldBe` expDto ^. newQuestionnaireUuid
  liftIO $ resDto ^. resolvedQuestionUuids `shouldBe` expDto ^. resolvedQuestionUuids

compareQtnMigratorDtos resDto expDto = do
  compareQuestionnaireCreateDtos' (resDto ^. oldQuestionnaire) (expDto ^. oldQuestionnaire)
  compareQuestionnaireCreateDtos' (resDto ^. newQuestionnaire) (expDto ^. newQuestionnaire)
  liftIO $ resDto ^. resolvedQuestionUuids `shouldBe` expDto ^. resolvedQuestionUuids
