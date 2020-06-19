module Wizard.Specs.API.Template.File.Common where

import Control.Lens ((^.))
import Data.Either (isRight)
import qualified Data.List as L
import Data.Maybe (isJust, isNothing)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Wizard.Database.DAO.Template.TemplateDAO

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertExistenceOfTemplateFileInDB appContext file tmlId = do
  eTemplate <- runInContextIO (findTemplateById tmlId) appContext
  liftIO $ isRight eTemplate `shouldBe` True
  let (Right templateFromDB) = eTemplate
  let mFile = L.find (\f -> f ^. uuid == file ^. uuid) (templateFromDB ^. files)
  liftIO $ isJust mFile `shouldBe` True
  let (Just fileFromDB) = mFile
  compareTemplateDtos fileFromDB file

assertAbsenceOfTemplateFileInDB appContext file tmlId = do
  eTemplate <- runInContextIO (findTemplateById tmlId) appContext
  liftIO $ isRight eTemplate `shouldBe` True
  let (Right templateFromDB) = eTemplate
  let mFile = L.find (\f -> f ^. uuid == file ^. uuid) (templateFromDB ^. files)
  liftIO $ isNothing mFile `shouldBe` True

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareTemplateDtos resDto expDto = do
  liftIO $ (resDto ^. fileName) `shouldBe` (expDto ^. fileName)
  liftIO $ (resDto ^. content) `shouldBe` (expDto ^. content)
