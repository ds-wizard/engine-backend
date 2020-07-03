module Wizard.Specs.API.Template.Asset.Common where

import Control.Lens ((^.))
import Data.Either (isRight)
import qualified Data.List as L
import Data.Maybe (isNothing)
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.Template.TemplateDAO

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertAbsenceOfTemplateAssetInDB appContext file tmlId = do
  eTemplate <- runInContextIO (findTemplateById tmlId) appContext
  liftIO $ isRight eTemplate `shouldBe` True
  let (Right templateFromDB) = eTemplate
  let mFile = L.find (\f -> f ^. uuid == file ^. uuid) (templateFromDB ^. assets)
  liftIO $ isNothing mFile `shouldBe` True

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareTemplateDtos resDto expDto = do
  liftIO $ (resDto ^. fileName) `shouldBe` (expDto ^. fileName)
  liftIO $ (resDto ^. contentType) `shouldBe` (expDto ^. contentType)
