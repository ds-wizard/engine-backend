module Specs.Service.Branch.BranchServiceSpec where

import Data.Maybe
import Test.Hspec

import Service.Branch.BranchService

branchServiceSpec =
  describe "Package Service" $
  it "isVersionInValidFormat" $ do
    isNothing (isValidArtifactId "core") `shouldBe` True
    isNothing (isValidArtifactId "ab") `shouldBe` True
    isNothing (isValidArtifactId "core-nl") `shouldBe` True
    isNothing (isValidArtifactId "core-nl-amsterdam") `shouldBe` True
    isJust (isValidArtifactId "a") `shouldBe` True
    isJust (isValidArtifactId "core.nl") `shouldBe` True
    isJust (isValidArtifactId "a.b") `shouldBe` True
    isJust (isValidArtifactId "core_nl") `shouldBe` True
