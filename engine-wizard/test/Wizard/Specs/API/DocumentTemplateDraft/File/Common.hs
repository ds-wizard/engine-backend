module Wizard.Specs.API.DocumentTemplateDraft.File.Common where

import Data.Either (isLeft)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateFileDAO
import Shared.Localization.Messages.Public
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Error.Error
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Model.App.App

import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfTemplateFileInDB appContext file documentTemplateId = do
  fileFromDB <- getOneFromDB (findFileById file.uuid) appContext
  compareTemplateFileDtos fileFromDB file

assertAbsenceOfTemplateFileInDB appContext file = do
  eFile <- runInContextIO (findFileById file.uuid) appContext
  liftIO $ isLeft eFile `shouldBe` True
  let (Left error) = eFile
  liftIO $
    error
      `shouldBe` NotExistsError
        ( _ERROR_DATABASE__ENTITY_NOT_FOUND
            "document_template_file"
            [("app_uuid", U.toString defaultApp.uuid), ("uuid", U.toString file.uuid)]
        )

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareTemplateFileDtos resDto expDto = do
  liftIO $ resDto.fileName `shouldBe` expDto.fileName
  liftIO $ resDto.content `shouldBe` expDto.content
