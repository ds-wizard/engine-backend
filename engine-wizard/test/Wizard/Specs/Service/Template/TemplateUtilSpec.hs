module Wizard.Specs.Service.Template.TemplateUtilSpec where

import Control.Lens ((^.))
import Data.Maybe (fromJust)
import Data.Time
import Test.Hspec

import LensesConfig hiding (templateMetamodelVersion)
import Shared.Constant.Template
import Shared.Model.Package.PackagePattern
import Shared.Model.Template.Template
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Service.Template.TemplateUtil

templateUtilSpec =
  describe "Template Utils" $ do
    let pkgId = "org.nl:core-nl:2.0.0"
    describe "filterTemplates" $ do
      it "No KM Specifications given => Deny" $
            -- GIVEN:
       do
        let templates =
              [ Template
                  { _templateTId = ""
                  , _templateName = ""
                  , _templateOrganizationId = ""
                  , _templateTemplateId = ""
                  , _templateVersion = ""
                  , _templateMetamodelVersion = templateMetamodelVersion
                  , _templateDescription = ""
                  , _templateReadme = ""
                  , _templateLicense = ""
                  , _templateAllowedPackages = []
                  , _templateRecommendedPackageId = Nothing
                  , _templateFormats = []
                  , _templateAppUuid = defaultApp ^. uuid
                  , _templateCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
                  }
              ]
            -- AND:
        let expectation = 0
            -- GIVEN:
        let result = filterTemplates (Just pkgId) templates
            -- THEN:
        length result `shouldBe` expectation
      it "One relevant KM Specifications given => Allow" $
            -- GIVEN:
       do
        let templates =
              [ Template
                  { _templateTId = ""
                  , _templateName = ""
                  , _templateOrganizationId = ""
                  , _templateTemplateId = ""
                  , _templateVersion = ""
                  , _templateMetamodelVersion = templateMetamodelVersion
                  , _templateDescription = ""
                  , _templateReadme = ""
                  , _templateLicense = ""
                  , _templateAllowedPackages =
                      [ PackagePattern
                          { _packagePatternOrgId = Nothing
                          , _packagePatternKmId = Nothing
                          , _packagePatternMinVersion = Nothing
                          , _packagePatternMaxVersion = Nothing
                          }
                      ]
                  , _templateRecommendedPackageId = Nothing
                  , _templateFormats = []
                  , _templateAppUuid = defaultApp ^. uuid
                  , _templateCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
                  }
              ]
            -- AND:
        let expectation = 1
            -- GIVEN:
        let result = filterTemplates (Just pkgId) templates
            -- THEN:
        length result `shouldBe` expectation
      it "One relevant and one non-relevant KM Specifications given => Allow" $
            -- GIVEN:
       do
        let templates =
              [ Template
                  { _templateTId = ""
                  , _templateName = ""
                  , _templateOrganizationId = ""
                  , _templateTemplateId = ""
                  , _templateVersion = ""
                  , _templateMetamodelVersion = templateMetamodelVersion
                  , _templateDescription = ""
                  , _templateReadme = ""
                  , _templateLicense = ""
                  , _templateAllowedPackages =
                      [ PackagePattern
                          { _packagePatternOrgId = Nothing
                          , _packagePatternKmId = Nothing
                          , _packagePatternMinVersion = Nothing
                          , _packagePatternMaxVersion = Nothing
                          }
                      , PackagePattern
                          { _packagePatternOrgId = Nothing
                          , _packagePatternKmId = Just "core-de"
                          , _packagePatternMinVersion = Nothing
                          , _packagePatternMaxVersion = Nothing
                          }
                      ]
                  , _templateRecommendedPackageId = Nothing
                  , _templateFormats = []
                  , _templateAppUuid = defaultApp ^. uuid
                  , _templateCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
                  }
              ]
            -- AND:
        let expectation = 1
            -- GIVEN:
        let result = filterTemplates (Just pkgId) templates
            -- THEN:
        length result `shouldBe` expectation
