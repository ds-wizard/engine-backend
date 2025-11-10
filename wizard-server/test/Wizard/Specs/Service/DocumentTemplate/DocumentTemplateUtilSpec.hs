module Wizard.Specs.Service.DocumentTemplate.DocumentTemplateUtilSpec where

import Data.Maybe (fromJust)
import Data.Time
import Test.Hspec

import Shared.DocumentTemplate.Constant.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackagePattern
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Model.Tenant.Tenant
import Wizard.Service.DocumentTemplate.DocumentTemplateUtil

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
                    , nonEditable = False
                    , tenantUuid = defaultTenant.uuid
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
                        [ KnowledgeModelPackagePattern
                            { orgId = Nothing
                            , kmId = Nothing
                            , minVersion = Nothing
                            , maxVersion = Nothing
                            }
                        ]
                    , nonEditable = False
                    , tenantUuid = defaultTenant.uuid
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
                        [ KnowledgeModelPackagePattern
                            { orgId = Nothing
                            , kmId = Nothing
                            , minVersion = Nothing
                            , maxVersion = Nothing
                            }
                        , KnowledgeModelPackagePattern
                            { orgId = Nothing
                            , kmId = Just "core-de"
                            , minVersion = Nothing
                            , maxVersion = Nothing
                            }
                        ]
                    , nonEditable = False
                    , tenantUuid = defaultTenant.uuid
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
