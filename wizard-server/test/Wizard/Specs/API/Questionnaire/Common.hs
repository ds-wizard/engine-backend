module Wizard.Specs.API.Questionnaire.Common where

import Data.Either (isLeft, isRight)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Database.DAO.Questionnaire.QuestionnaireDAO
import Wizard.Database.DAO.Questionnaire.QuestionnaireEventDAO
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Tenant.Config.ConfigMapper
import Wizard.Service.Tenant.Config.ConfigService

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfQuestionnaireInDB appContext qtn qtnEvents = do
  eQtn <- runInContextIO (findQuestionnaireByUuid qtn.uuid) appContext
  liftIO $ isRight eQtn `shouldBe` True
  let (Right qtnFromDb) = eQtn
  compareQuestionnaireDtos qtnFromDb qtn
  eQtnEvents <- runInContextIO (findQuestionnaireEventsByQuestionnaireUuid qtn.uuid) appContext
  liftIO $ isRight eQtnEvents `shouldBe` True
  let (Right qtnEventsFromDb) = eQtnEvents
  liftIO $ qtnEventsFromDb `shouldBe` qtnEvents

assertExistenceOfQuestionnaireContentInDB appContext qtnUuid content = do
  eQtnEvents <- runInContextIO (findQuestionnaireEventsByQuestionnaireUuid qtnUuid) appContext
  liftIO $ isRight eQtnEvents `shouldBe` True
  let (Right qtnEventsFromDb) = eQtnEvents
  compareQuestionnaireContentDtos qtnEventsFromDb content

assertAbsenceOfQuestionnaireInDB appContext qtn = do
  eQtn <- runInContextIO (findQuestionnaireByUuid qtn.uuid) appContext
  liftIO $ isLeft eQtn `shouldBe` True
  let (Left error) = eQtn
  liftIO $
    error
      `shouldBe` NotExistsError
        ( _ERROR_DATABASE__ENTITY_NOT_FOUND
            "questionnaire"
            [("tenant_uuid", U.toString defaultTenant.uuid), ("uuid", U.toString qtn.uuid)]
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

compareQuestionnaireCreateDtos'' resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.phaseUuid `shouldBe` expDto.phaseUuid
  liftIO $ resDto.visibility `shouldBe` expDto.visibility
  liftIO $ resDto.sharing `shouldBe` expDto.sharing
  liftIO $ resDto.selectedQuestionTagUuids `shouldBe` expDto.selectedQuestionTagUuids
  liftIO $ resDto.knowledgeModel `shouldBe` expDto.knowledgeModel
  liftIO $ resDto.replies `shouldBe` expDto.replies

compareQuestionnaireDtos resDto expDto = liftIO $ resDto `shouldBe` expDto

compareQuestionnaireContentDtos resDto expDto =
  liftIO $ resDto `shouldBe` expDto

compareReportDtos resDto expDto = do
  liftIO $ resDto.totalReport `shouldBe` expDto.totalReport
  liftIO $ resDto.chapterReports `shouldBe` expDto.chapterReports

-- --------------------------------
-- HELPERS
-- --------------------------------
updateAnonymousQuestionnaireSharing appContext value = do
  (Right tenantConfig) <- runInContextIO getCurrentTenantConfig appContext
  let updatedTenantConfig = tenantConfig {questionnaire = tenantConfig.questionnaire {questionnaireSharing = tenantConfig.questionnaire.questionnaireSharing {anonymousEnabled = value}}}
  runInContextIO (modifyTenantConfigDto (toChangeDTO updatedTenantConfig)) appContext
