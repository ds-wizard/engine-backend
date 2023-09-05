module Wizard.Specs.Service.DocumentTemplate.DocumentTemplateUtilSpec where

import Data.Maybe (fromJust)
import Data.Time
import Test.Hspec

import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Model.App.App
import Wizard.Service.DocumentTemplate.DocumentTemplateUtil
import WizardLib.DocumentTemplate.Constant.DocumentTemplate
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.KnowledgeModel.Model.Package.PackagePattern

documentTemplateUtilSpec =
  describe "Document DocumentTemplate Utils" $ do
    let pkgId = "org.nl:core-nl:2.0.0"
    describe "filterDocumentTemplates" $ do
      it "No KM Specifications given => Deny" $
        -- GIVEN:
        do
          let templates =
                [ DocumentTemplate
                    { tId = ""
                    , name = ""
                    , organizationId = ""
                    , templateId = ""
                    , version = ""
                    , phase = ReleasedDocumentTemplatePhase
                    , metamodelVersion = documentTemplateMetamodelVersion
                    , description = ""
                    , readme = ""
                    , license = ""
                    , allowedPackages = []
                    , formats = []
                    , nonEditable = False
                    , appUuid = defaultApp.uuid
                    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
                    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
                    }
                ]
          -- AND:
          let expectation = 0
          -- GIVEN:
          let result = filterDocumentTemplates (Just pkgId) templates
          -- THEN:
          length result `shouldBe` expectation
      it "One relevant KM Specifications given => Allow" $
        -- GIVEN:
        do
          let templates =
                [ DocumentTemplate
                    { tId = ""
                    , name = ""
                    , organizationId = ""
                    , templateId = ""
                    , version = ""
                    , phase = ReleasedDocumentTemplatePhase
                    , metamodelVersion = documentTemplateMetamodelVersion
                    , description = ""
                    , readme = ""
                    , license = ""
                    , allowedPackages =
                        [ PackagePattern
                            { orgId = Nothing
                            , kmId = Nothing
                            , minVersion = Nothing
                            , maxVersion = Nothing
                            }
                        ]
                    , formats = []
                    , nonEditable = False
                    , appUuid = defaultApp.uuid
                    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
                    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
                    }
                ]
          -- AND:
          let expectation = 1
          -- GIVEN:
          let result = filterDocumentTemplates (Just pkgId) templates
          -- THEN:
          length result `shouldBe` expectation
      it "One relevant and one non-relevant KM Specifications given => Allow" $
        -- GIVEN:
        do
          let templates =
                [ DocumentTemplate
                    { tId = ""
                    , name = ""
                    , organizationId = ""
                    , templateId = ""
                    , version = ""
                    , phase = ReleasedDocumentTemplatePhase
                    , metamodelVersion = documentTemplateMetamodelVersion
                    , description = ""
                    , readme = ""
                    , license = ""
                    , allowedPackages =
                        [ PackagePattern
                            { orgId = Nothing
                            , kmId = Nothing
                            , minVersion = Nothing
                            , maxVersion = Nothing
                            }
                        , PackagePattern
                            { orgId = Nothing
                            , kmId = Just "core-de"
                            , minVersion = Nothing
                            , maxVersion = Nothing
                            }
                        ]
                    , formats = []
                    , nonEditable = False
                    , appUuid = defaultApp.uuid
                    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
                    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
                    }
                ]
          -- AND:
          let expectation = 1
          -- GIVEN:
          let result = filterDocumentTemplates (Just pkgId) templates
          -- THEN:
          length result `shouldBe` expectation
