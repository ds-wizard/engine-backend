module Wizard.Specs.Service.Template.TemplateUtilSpec where

import Data.Maybe (fromJust)
import Data.Time
import Test.Hspec

import Shared.Constant.Template
import Shared.Model.Package.PackagePattern
import Shared.Model.Template.Template
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Model.App.App
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
                    { tId = ""
                    , name = ""
                    , organizationId = ""
                    , templateId = ""
                    , version = ""
                    , metamodelVersion = templateMetamodelVersion
                    , description = ""
                    , readme = ""
                    , license = ""
                    , allowedPackages = []
                    , recommendedPackageId = Nothing
                    , formats = []
                    , appUuid = defaultApp.uuid
                    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
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
                    { tId = ""
                    , name = ""
                    , organizationId = ""
                    , templateId = ""
                    , version = ""
                    , metamodelVersion = templateMetamodelVersion
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
                    , recommendedPackageId = Nothing
                    , formats = []
                    , appUuid = defaultApp.uuid
                    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
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
                    { tId = ""
                    , name = ""
                    , organizationId = ""
                    , templateId = ""
                    , version = ""
                    , metamodelVersion = templateMetamodelVersion
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
                    , recommendedPackageId = Nothing
                    , formats = []
                    , appUuid = defaultApp.uuid
                    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 21) 0
                    }
                ]
          -- AND:
          let expectation = 1
          -- GIVEN:
          let result = filterTemplates (Just pkgId) templates
          -- THEN:
          length result `shouldBe` expectation
