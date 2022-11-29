module Wizard.Specs.API.Questionnaire.Common where

import Data.Either (isLeft, isRight)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Model.App.App
import Wizard.Model.Config.AppConfig
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Service.Config.AppConfigMapper
import Wizard.Service.Config.AppConfigService

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfQuestionnaireInDB appContext qtn = do
  eQtn <- runInContextIO (findQuestionnaireById (U.toString qtn.uuid)) appContext
  liftIO $ isRight eQtn `shouldBe` True
  let (Right qtnFromDb) = eQtn
  compareQuestionnaireDtos qtnFromDb qtn

assertExistenceOfQuestionnaireContentInDB appContext qtnUuid content = do
  eQtn <- runInContextIO (findQuestionnaireById (U.toString qtnUuid)) appContext
  liftIO $ isRight eQtn `shouldBe` True
  let (Right qtnFromDb) = eQtn
  compareQuestionnaireContentDtos qtnFromDb content

assertAbsenceOfQuestionnaireInDB appContext qtn = do
  let qtnUuid = U.toString $ qtn.uuid
  eQtn <- runInContextIO (findQuestionnaireById qtnUuid) appContext
  liftIO $ isLeft eQtn `shouldBe` True
  let (Left error) = eQtn
  liftIO $
    error
      `shouldBe` NotExistsError
        ( _ERROR_DATABASE__ENTITY_NOT_FOUND
            "questionnaire"
            [("app_uuid", U.toString defaultApp.uuid), ("uuid", qtnUuid)]
        )

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareQuestionnaireCreateDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.visibility `shouldBe` expDto.visibility
  liftIO $ resDto.sharing `shouldBe` expDto.sharing
  liftIO $ resDto.package `shouldBe` expDto.package

compareQuestionnaireCreateFromTemplateDtos resDto expDto = do
  liftIO $ resDto.uuid `shouldNotBe` expDto.uuid
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.visibility `shouldBe` expDto.visibility
  liftIO $ resDto.sharing `shouldBe` expDto.sharing
  liftIO $ resDto.state `shouldBe` expDto.state
  liftIO $ resDto.package `shouldBe` expDto.package

compareQuestionnaireCloneDtos resDto expDto = do
  liftIO $ resDto.uuid `shouldNotBe` expDto.uuid
  liftIO $ resDto.name `shouldBe` ("Copy of " ++ expDto.name)
  liftIO $ resDto.visibility `shouldBe` expDto.visibility
  liftIO $ resDto.sharing `shouldBe` expDto.sharing
  liftIO $ resDto.state `shouldBe` expDto.state
  liftIO $ resDto.package `shouldBe` expDto.package

compareQuestionnaireCreateDtos' resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.phaseUuid `shouldBe` expDto.phaseUuid
  liftIO $ resDto.visibility `shouldBe` expDto.visibility
  liftIO $ resDto.sharing `shouldBe` expDto.sharing
  liftIO $ resDto.state `shouldBe` expDto.state
  liftIO $ resDto.package `shouldBe` expDto.package
  liftIO $ resDto.selectedQuestionTagUuids `shouldBe` expDto.selectedQuestionTagUuids
  liftIO $ resDto.knowledgeModel `shouldBe` expDto.knowledgeModel
  liftIO $ resDto.replies `shouldBe` expDto.replies

compareQuestionnaireDtos resDto expDto = liftIO $ resDto `shouldBe` expDto

compareQuestionnaireContentDtos resDto expDto = liftIO $ resDto.events `shouldBe` expDto.events

compareReportDtos resDto expDto = do
  liftIO $ resDto.totalReport `shouldBe` expDto.totalReport
  liftIO $ resDto.chapterReports `shouldBe` expDto.chapterReports

-- --------------------------------
-- HELPERS
-- --------------------------------
updateAnonymousQuestionnaireSharing appContext value = do
  (Right appConfig) <- runInContextIO getAppConfig appContext
  let updatedAppConfig = appConfig {questionnaire = appConfig.questionnaire {questionnaireSharing = appConfig.questionnaire.questionnaireSharing {anonymousEnabled = value}}}
  runInContextIO (modifyAppConfigDto (toChangeDTO updatedAppConfig)) appContext
