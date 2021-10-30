module Wizard.Specs.API.Template.Asset.Common where

import Control.Lens ((^.))
import Data.Either (isLeft)
import qualified Data.UUID as U
import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import LensesConfig hiding (request)
import Shared.Api.Resource.Error.ErrorJM ()
import Shared.Database.DAO.Template.TemplateAssetDAO
import Shared.Localization.Messages.Public
import Shared.Model.Error.Error
import Wizard.Database.Migration.Development.App.Data.Apps

import Wizard.Specs.Common

-- --------------------------------
-- ASSERTS
-- --------------------------------
assertAbsenceOfTemplateAssetInDB appContext asset = do
  let assetUuid = U.toString $ asset ^. uuid
  eAsset <- runInContextIO (findTemplateAssetById assetUuid) appContext
  liftIO $ isLeft eAsset `shouldBe` True
  let (Left error) = eAsset
  liftIO $
    error `shouldBe`
    NotExistsError
      (_ERROR_DATABASE__ENTITY_NOT_FOUND
         "template_asset"
         [("app_uuid", U.toString $ defaultApp ^. uuid), ("uuid", assetUuid)])

-- --------------------------------
-- COMPARATORS
-- --------------------------------
compareTemplateDtos resDto expDto = do
  liftIO $ (resDto ^. fileName) `shouldBe` (expDto ^. fileName)
  liftIO $ (resDto ^. contentType) `shouldBe` (expDto ^. contentType)
