module Wizard.Specs.Service.Package.PackageUtilSpec where

import Test.Hspec

import Shared.Model.Package.PackagePattern
import Shared.Service.Package.PackageUtil

packageUtilSpec =
  describe "Package Utils" $ do
    let pkgIdSplit = ["org.nl", "core-nl", "2.0.0"]
    describe "fitsIntoKMSpec" $ do
      it "No restrictions => Allow anything" $
        -- GIVEN:
       do
        let kmSpec =
              PackagePattern
                { _packagePatternOrgId = Nothing
                , _packagePatternKmId = Nothing
                , _packagePatternMinVersion = Nothing
                , _packagePatternMaxVersion = Nothing
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
                { _packagePatternOrgId = Just "org.nl"
                , _packagePatternKmId = Nothing
                , _packagePatternMinVersion = Nothing
                , _packagePatternMaxVersion = Nothing
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
                { _packagePatternOrgId = Just "org.de"
                , _packagePatternKmId = Nothing
                , _packagePatternMinVersion = Nothing
                , _packagePatternMaxVersion = Nothing
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
                { _packagePatternOrgId = Nothing
                , _packagePatternKmId = Just "core-nl"
                , _packagePatternMinVersion = Nothing
                , _packagePatternMaxVersion = Nothing
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
                { _packagePatternOrgId = Nothing
                , _packagePatternKmId = Just "core-de"
                , _packagePatternMinVersion = Nothing
                , _packagePatternMaxVersion = Nothing
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
                { _packagePatternOrgId = Nothing
                , _packagePatternKmId = Nothing
                , _packagePatternMinVersion = Just "1.0.0"
                , _packagePatternMaxVersion = Nothing
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
                { _packagePatternOrgId = Nothing
                , _packagePatternKmId = Nothing
                , _packagePatternMinVersion = Just "2.0.1"
                , _packagePatternMaxVersion = Nothing
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
                { _packagePatternOrgId = Nothing
                , _packagePatternKmId = Nothing
                , _packagePatternMinVersion = Nothing
                , _packagePatternMaxVersion = Just "2.0.1"
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
                { _packagePatternOrgId = Nothing
                , _packagePatternKmId = Nothing
                , _packagePatternMinVersion = Nothing
                , _packagePatternMaxVersion = Just "1.0.0"
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
                { _packagePatternOrgId = Just "org.de"
                , _packagePatternKmId = Just "core-nl"
                , _packagePatternMinVersion = Nothing
                , _packagePatternMaxVersion = Nothing
                }
        -- GIVEN:
        let result = fitsIntoKMSpec pkgIdSplit kmSpec
        -- THEN:
        result `shouldBe` False
