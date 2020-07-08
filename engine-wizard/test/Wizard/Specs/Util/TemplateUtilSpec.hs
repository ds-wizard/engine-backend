module Wizard.Specs.Util.TemplateUtilSpec where

import Data.Maybe (fromJust)
import Data.Time
import Test.Hspec

import Shared.Model.Template.Template
import Wizard.Service.Template.TemplateUtil

templateUtilSpec =
  describe "Template Utils" $ do
    let pkgId = "org.nl:core-nl:2.0.0"
    let pkgIdSplit = ["org.nl", "core-nl", "2.0.0"]
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
                  , _templateMetamodelVersion = 1
                  , _templateDescription = ""
                  , _templateReadme = ""
                  , _templateLicense = ""
                  , _templateAllowedPackages = []
                  , _templateRecommendedPackageId = Nothing
                  , _templateFormats = []
                  , _templateFiles = []
                  , _templateAssets = []
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
                  , _templateMetamodelVersion = 1
                  , _templateDescription = ""
                  , _templateReadme = ""
                  , _templateLicense = ""
                  , _templateAllowedPackages =
                      [ TemplateAllowedPackage
                          { _templateAllowedPackageOrgId = Nothing
                          , _templateAllowedPackageKmId = Nothing
                          , _templateAllowedPackageMinVersion = Nothing
                          , _templateAllowedPackageMaxVersion = Nothing
                          }
                      ]
                  , _templateRecommendedPackageId = Nothing
                  , _templateFormats = []
                  , _templateFiles = []
                  , _templateAssets = []
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
                  , _templateMetamodelVersion = 1
                  , _templateDescription = ""
                  , _templateReadme = ""
                  , _templateLicense = ""
                  , _templateAllowedPackages =
                      [ TemplateAllowedPackage
                          { _templateAllowedPackageOrgId = Nothing
                          , _templateAllowedPackageKmId = Nothing
                          , _templateAllowedPackageMinVersion = Nothing
                          , _templateAllowedPackageMaxVersion = Nothing
                          }
                      , TemplateAllowedPackage
                          { _templateAllowedPackageOrgId = Nothing
                          , _templateAllowedPackageKmId = Just "core-de"
                          , _templateAllowedPackageMinVersion = Nothing
                          , _templateAllowedPackageMaxVersion = Nothing
                          }
                      ]
                  , _templateRecommendedPackageId = Nothing
                  , _templateFormats = []
                  , _templateFiles = []
                  , _templateAssets = []
                  , _templateCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
                  }
              ]
            -- AND:
        let expectation = 1
            -- GIVEN:
        let result = filterTemplates (Just pkgId) templates
            -- THEN:
        length result `shouldBe` expectation
    describe "fitsIntoKMSpec" $ do
      it "No restrictions => Allow anything" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage
                { _templateAllowedPackageOrgId = Nothing
                , _templateAllowedPackageKmId = Nothing
                , _templateAllowedPackageMinVersion = Nothing
                , _templateAllowedPackageMaxVersion = Nothing
                }
        -- GIVEN:
        let result = fitsIntoKMSpec pkgIdSplit kmSpec
        -- THEN:
        result `shouldBe` True
      it "Restriction on 'orgId', same provided 'orgId' => Allow" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage
                { _templateAllowedPackageOrgId = Just "org.nl"
                , _templateAllowedPackageKmId = Nothing
                , _templateAllowedPackageMinVersion = Nothing
                , _templateAllowedPackageMaxVersion = Nothing
                }
        -- GIVEN:
        let result = fitsIntoKMSpec pkgIdSplit kmSpec
        -- THEN:
        result `shouldBe` True
      it "Restriction on 'orgId', different provided 'orgId' => Deny" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage
                { _templateAllowedPackageOrgId = Just "org.de"
                , _templateAllowedPackageKmId = Nothing
                , _templateAllowedPackageMinVersion = Nothing
                , _templateAllowedPackageMaxVersion = Nothing
                }
        -- GIVEN:
        let result = fitsIntoKMSpec pkgIdSplit kmSpec
        -- THEN:
        result `shouldBe` False
      it "Restriction on 'KmID', same provided 'KmID' => Allow" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage
                { _templateAllowedPackageOrgId = Nothing
                , _templateAllowedPackageKmId = Just "core-nl"
                , _templateAllowedPackageMinVersion = Nothing
                , _templateAllowedPackageMaxVersion = Nothing
                }
        -- GIVEN:
        let result = fitsIntoKMSpec pkgIdSplit kmSpec
        -- THEN:
        result `shouldBe` True
      it "Restriction on 'KmID', different provided 'KmID' => Deny" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage
                { _templateAllowedPackageOrgId = Nothing
                , _templateAllowedPackageKmId = Just "core-de"
                , _templateAllowedPackageMinVersion = Nothing
                , _templateAllowedPackageMaxVersion = Nothing
                }
        -- GIVEN:
        let result = fitsIntoKMSpec pkgIdSplit kmSpec
        -- THEN:
        result `shouldBe` False
      it "Restriction on 'minimal version', provided higher version => Alow" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage
                { _templateAllowedPackageOrgId = Nothing
                , _templateAllowedPackageKmId = Nothing
                , _templateAllowedPackageMinVersion = Just "1.0.0"
                , _templateAllowedPackageMaxVersion = Nothing
                }
        -- GIVEN:
        let result = fitsIntoKMSpec pkgIdSplit kmSpec
        -- THEN:
        result `shouldBe` True
      it "Restriction on 'minimal version', provided lower version => Deny" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage
                { _templateAllowedPackageOrgId = Nothing
                , _templateAllowedPackageKmId = Nothing
                , _templateAllowedPackageMinVersion = Just "2.0.1"
                , _templateAllowedPackageMaxVersion = Nothing
                }
        -- GIVEN:
        let result = fitsIntoKMSpec pkgIdSplit kmSpec
        -- THEN:
        result `shouldBe` False
      it "Restriction on 'maximal version', provided lower version => Alow" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage
                { _templateAllowedPackageOrgId = Nothing
                , _templateAllowedPackageKmId = Nothing
                , _templateAllowedPackageMinVersion = Nothing
                , _templateAllowedPackageMaxVersion = Just "2.0.1"
                }
        -- GIVEN:
        let result = fitsIntoKMSpec pkgIdSplit kmSpec
        -- THEN:
        result `shouldBe` True
      it "Restriction on 'maximal version', provided higher version => Deny" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage
                { _templateAllowedPackageOrgId = Nothing
                , _templateAllowedPackageKmId = Nothing
                , _templateAllowedPackageMinVersion = Nothing
                , _templateAllowedPackageMaxVersion = Just "1.0.0"
                }
        -- GIVEN:
        let result = fitsIntoKMSpec pkgIdSplit kmSpec
        -- THEN:
        result `shouldBe` False
      it "Two restrictions, provided data satisfies just one => Deny" $
        -- GIVEN:
       do
        let kmSpec =
              TemplateAllowedPackage
                { _templateAllowedPackageOrgId = Just "org.de"
                , _templateAllowedPackageKmId = Just "core-nl"
                , _templateAllowedPackageMinVersion = Nothing
                , _templateAllowedPackageMaxVersion = Nothing
                }
        -- GIVEN:
        let result = fitsIntoKMSpec pkgIdSplit kmSpec
        -- THEN:
        result `shouldBe` False
