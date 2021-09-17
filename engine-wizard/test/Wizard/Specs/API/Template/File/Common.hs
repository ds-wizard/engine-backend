module Wizard.Specs.API.Template.File.Common where

import Control.Lens ((^.))
import Data.Either (isLeft)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.Template.TemplateFileDAO
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error

import Wizard.Specs.API.Common
import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfTemplateFileInDB appContext file tmlId = do
  let fileUuid = U.toString $ file ^. uuid
  fileFromDB <- getOneFromDB (findTemplateFileById fileUuid) appContext
  compareTemplateFileDtos fileFromDB file

assertAbsenceOfTemplateFileInDB appContext file = do
  let fileUuid = U.toString $ file ^. uuid
  eFile <- runInContextIO (findTemplateFileById fileUuid) appContext
  liftIO $ isLeft eFile `shouldBe` True
  let (Left error) = eFile
  liftIO $ error `shouldBe` NotExistsError (_ERROR_DATABASE__ENTITY_NOT_FOUND "template_file" fileUuid)

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareTemplateFileDtos resDto expDto = do
  liftIO $ (resDto ^. fileName) `shouldBe` (expDto ^. fileName)
  liftIO $ (resDto ^. content) `shouldBe` (expDto ^. content)
