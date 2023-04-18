module Wizard.Specs.Service.Package.PackageUtilSpec where

import Test.Hspec

import WizardLib.KnowledgeModel.Model.Package.PackagePattern
import WizardLib.KnowledgeModel.Service.Package.PackageUtil

packageUtilSpec =
  describe "Package Utils" $ do
    let pkgIdSplit = ["org.nl", "core-nl", "2.0.0"]
    describe "fitsIntoKMSpec" $ do
      it "No restrictions => Allow anything" $
        -- GIVEN:
        do
          let kmSpec =
                PackagePattern
                  { orgId = Nothing
                  , kmId = Nothing
                  , minVersion = Nothing
                  , maxVersion = Nothing
                  }
          -- GIVEN:
          let result = fitsIntoKMSpec pkgIdSplit kmSpec
          -- THEN:
          result `shouldBe` True
      it "Restriction on 'orgId', same provided 'orgId' => Allow" $
        -- GIVEN:
        do
          let kmSpec =
                PackagePattern
                  { orgId = Just "org.nl"
                  , kmId = Nothing
                  , minVersion = Nothing
                  , maxVersion = Nothing
                  }
          -- GIVEN:
          let result = fitsIntoKMSpec pkgIdSplit kmSpec
          -- THEN:
          result `shouldBe` True
      it "Restriction on 'orgId', different provided 'orgId' => Deny" $
        -- GIVEN:
        do
          let kmSpec =
                PackagePattern
                  { orgId = Just "org.de"
                  , kmId = Nothing
                  , minVersion = Nothing
                  , maxVersion = Nothing
                  }
          -- GIVEN:
          let result = fitsIntoKMSpec pkgIdSplit kmSpec
          -- THEN:
          result `shouldBe` False
      it "Restriction on 'KmID', same provided 'KmID' => Allow" $
        -- GIVEN:
        do
          let kmSpec =
                PackagePattern
                  { orgId = Nothing
                  , kmId = Just "core-nl"
                  , minVersion = Nothing
                  , maxVersion = Nothing
                  }
          -- GIVEN:
          let result = fitsIntoKMSpec pkgIdSplit kmSpec
          -- THEN:
          result `shouldBe` True
      it "Restriction on 'KmID', different provided 'KmID' => Deny" $
        -- GIVEN:
        do
          let kmSpec =
                PackagePattern
                  { orgId = Nothing
                  , kmId = Just "core-de"
                  , minVersion = Nothing
                  , maxVersion = Nothing
                  }
          -- GIVEN:
          let result = fitsIntoKMSpec pkgIdSplit kmSpec
          -- THEN:
          result `shouldBe` False
      it "Restriction on 'minimal version', provided higher version => Alow" $
        -- GIVEN:
        do
          let kmSpec =
                PackagePattern
                  { orgId = Nothing
                  , kmId = Nothing
                  , minVersion = Just "1.0.0"
                  , maxVersion = Nothing
                  }
          -- GIVEN:
          let result = fitsIntoKMSpec pkgIdSplit kmSpec
          -- THEN:
          result `shouldBe` True
      it "Restriction on 'minimal version', provided lower version => Deny" $
        -- GIVEN:
        do
          let kmSpec =
                PackagePattern
                  { orgId = Nothing
                  , kmId = Nothing
                  , minVersion = Just "2.0.1"
                  , maxVersion = Nothing
                  }
          -- GIVEN:
          let result = fitsIntoKMSpec pkgIdSplit kmSpec
          -- THEN:
          result `shouldBe` False
      it "Restriction on 'maximal version', provided lower version => Alow" $
        -- GIVEN:
        do
          let kmSpec =
                PackagePattern
                  { orgId = Nothing
                  , kmId = Nothing
                  , minVersion = Nothing
                  , maxVersion = Just "2.0.1"
                  }
          -- GIVEN:
          let result = fitsIntoKMSpec pkgIdSplit kmSpec
          -- THEN:
          result `shouldBe` True
      it "Restriction on 'maximal version', provided higher version => Deny" $
        -- GIVEN:
        do
          let kmSpec =
                PackagePattern
                  { orgId = Nothing
                  , kmId = Nothing
                  , minVersion = Nothing
                  , maxVersion = Just "1.0.0"
                  }
          -- GIVEN:
          let result = fitsIntoKMSpec pkgIdSplit kmSpec
          -- THEN:
          result `shouldBe` False
      it "Two restrictions, provided data satisfies just one => Deny" $
        -- GIVEN:
        do
          let kmSpec =
                PackagePattern
                  { orgId = Just "org.de"
                  , kmId = Just "core-nl"
                  , minVersion = Nothing
                  , maxVersion = Nothing
                  }
          -- GIVEN:
          let result = fitsIntoKMSpec pkgIdSplit kmSpec
          -- THEN:
          result `shouldBe` False
