module Wizard.Specs.Service.KnowledgeModel.Locale.Pot.PotFileServiceSpec where

import qualified Data.List as L
import Data.Maybe (fromJust)
import Data.Time
import Test.Hspec

import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Service.KnowledgeModel.Locale.Pot.PotFileService
import Wizard.Util.Gettext

potFileServiceSpec =
  describe "Pot.PotFileService" $ do
    let now = UTCTime (fromJust $ fromGregorianValid 2026 7 14) 0
    describe "buildTranslationTemplate" $ do
      it "builds a POT with headers and entries from the knowledge model" $ do
        -- WHEN:
        let pot = buildTranslationTemplate globalKmPackage km1 now
        -- THEN:
        pot `shouldSatisfy` L.isInfixOf "\"Project-Id-Version: global:core:1.0.0\\n\""
        pot `shouldSatisfy` L.isInfixOf "\"POT-Creation-Date: 2026-07-14 00:00+0000\\n\""
        pot `shouldSatisfy` L.isInfixOf "\"Language: en\\n\""
        pot `shouldSatisfy` L.isInfixOf "\"Plural-Forms: nplurals=2; plural=(n != 1);\\n\""
        pot `shouldSatisfy` L.isInfixOf "#: chapter/00000000-0000-0000-0000-0000000000c1/title"
        pot `shouldSatisfy` L.isInfixOf "msgid \"Design of experiment\""
      it "extracts entries in document order without empty strings" $ do
        -- WHEN:
        let entries = buildPotEntries km1
        -- THEN:
        entries `shouldSatisfy` (not . any (null . (.msgid)))
        fmap (.msgid) (take 1 entries) `shouldBe` ["Design of experiment"]
    describe "dedupeEntries" $
      it "merges duplicate msgids and keeps the first occurrence order" $ do
        -- GIVEN:
        let entries =
              [ PotEntry {references = ["chapter/a/title"], msgid = "Title"}
              , PotEntry {references = ["question/b/title"], msgid = "Other"}
              , PotEntry {references = ["chapter/c/title"], msgid = "Title"}
              ]
        -- WHEN:
        let result = dedupeEntries entries
        -- THEN:
        result
          `shouldBe` [ PotEntry {references = ["chapter/a/title", "chapter/c/title"], msgid = "Title"}
                     , PotEntry {references = ["question/b/title"], msgid = "Other"}
                     ]
