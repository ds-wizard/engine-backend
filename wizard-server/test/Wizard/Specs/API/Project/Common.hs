module Wizard.Specs.API.Project.Common where

import Data.Either (isLeft, isRight)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Common.Api.Resource.Error.ErrorJM ()
import Shared.Common.Localization.Messages.Public
import Shared.Common.Model.Error.Error
import Wizard.Database.DAO.Project.ProjectDAO
import Wizard.Database.DAO.Project.ProjectEventDAO
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Project.Project
import Wizard.Model.Tenant.Config.TenantConfig
import Wizard.Model.Tenant.Tenant
import Wizard.Service.Tenant.Config.ConfigService

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfProjectInDB appContext project projectEvents = do
  eProject <- runInContextIO (findProjectByUuid project.uuid) appContext
  liftIO $ isRight eProject `shouldBe` True
  let (Right projectFromDb) = eProject
  compareProjectDtos projectFromDb project
  eProjectEvents <- runInContextIO (findProjectEventsByProjectUuid project.uuid) appContext
  liftIO $ isRight eProjectEvents `shouldBe` True
  let (Right projectEventsFromDb) = eProjectEvents
  liftIO $ projectEventsFromDb `shouldBe` projectEvents

assertExistenceOfProjectContentInDB appContext projectUuid content = do
  eProjectEvents <- runInContextIO (findProjectEventsByProjectUuid projectUuid) appContext
  liftIO $ isRight eProjectEvents `shouldBe` True
  let (Right projectEventsFromDb) = eProjectEvents
  compareProjectContentDtos projectEventsFromDb content

assertAbsenceOfProjectInDB appContext project = do
  eProject <- runInContextIO (findProjectByUuid project.uuid) appContext
  liftIO $ isLeft eProject `shouldBe` True
  let (Left error) = eProject
  liftIO $
    error
      `shouldBe` NotExistsError
        ( _ERROR_DATABASE__ENTITY_NOT_FOUND
            "project"
            [("tenant_uuid", U.toString defaultTenant.uuid), ("uuid", U.toString project.uuid)]
        )

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareProjectCreateDtos resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.visibility `shouldBe` expDto.visibility
  liftIO $ resDto.sharing `shouldBe` expDto.sharing
  liftIO $ resDto.knowledgeModelPackage `shouldBe` expDto.knowledgeModelPackage

compareProjectCreateFromTemplateDtos resDto expDto = do
  liftIO $ resDto.uuid `shouldNotBe` expDto.uuid
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.visibility `shouldBe` expDto.visibility
  liftIO $ resDto.sharing `shouldBe` expDto.sharing
  liftIO $ resDto.state `shouldBe` expDto.state
  liftIO $ resDto.knowledgeModelPackage `shouldBe` expDto.knowledgeModelPackage

compareProjectCloneDtos resDto expDto = do
  liftIO $ resDto.uuid `shouldNotBe` expDto.uuid
  liftIO $ resDto.name `shouldBe` ("Copy of " ++ expDto.name)
  liftIO $ resDto.visibility `shouldBe` expDto.visibility
  liftIO $ resDto.sharing `shouldBe` expDto.sharing
  liftIO $ resDto.state `shouldBe` expDto.state
  liftIO $ resDto.knowledgeModelPackage `shouldBe` expDto.knowledgeModelPackage

compareProjectCreateDtos' resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.phaseUuid `shouldBe` expDto.phaseUuid
  liftIO $ resDto.visibility `shouldBe` expDto.visibility
  liftIO $ resDto.sharing `shouldBe` expDto.sharing
  liftIO $ resDto.state `shouldBe` expDto.state
  liftIO $ resDto.knowledgeModelPackage `shouldBe` expDto.knowledgeModelPackage
  liftIO $ resDto.selectedQuestionTagUuids `shouldBe` expDto.selectedQuestionTagUuids
  liftIO $ resDto.knowledgeModel `shouldBe` expDto.knowledgeModel
  liftIO $ resDto.replies `shouldBe` expDto.replies

compareProjectCreateDtos'' resDto expDto = do
  liftIO $ resDto.name `shouldBe` expDto.name
  liftIO $ resDto.phaseUuid `shouldBe` expDto.phaseUuid
  liftIO $ resDto.visibility `shouldBe` expDto.visibility
  liftIO $ resDto.sharing `shouldBe` expDto.sharing
  liftIO $ resDto.selectedQuestionTagUuids `shouldBe` expDto.selectedQuestionTagUuids
  liftIO $ resDto.knowledgeModel `shouldBe` expDto.knowledgeModel
  liftIO $ resDto.replies `shouldBe` expDto.replies

compareProjectDtos resDto expDto = liftIO $ resDto `shouldBe` expDto

compareProjectContentDtos resDto expDto =
  liftIO $ resDto `shouldBe` expDto

compareReportDtos resDto expDto = do
  liftIO $ resDto.totalReport `shouldBe` expDto.totalReport
  liftIO $ resDto.chapterReports `shouldBe` expDto.chapterReports

-- --------------------------------
-- HELPERS
-- --------------------------------
updateAnonymousProjectSharing appContext value = do
  (Right tcProject) <- runInContextIO getCurrentTenantConfigProject appContext
  let tcProjectUpdated = tcProject {projectSharing = tcProject.projectSharing {anonymousEnabled = value}}
  runInContextIO (modifyTenantConfigProject tcProjectUpdated) appContext
