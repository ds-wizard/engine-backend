module Wizard.Specs.Util.GettextSpec where

import qualified Data.List as L
import Test.Hspec

import Wizard.Util.Gettext

gettextSpec =
  describe "Gettext" $ do
    describe "serializePot" $ do
      it "serializes header and entries" $ do
        -- GIVEN:
        let headers = [("Project-Id-Version", "global:core:1.0.0"), ("Language", "en")]
        let entries =
              [ PotEntry {references = ["chapter/5c135d55-1746-4a51-a739-7d495ac4a2ae/title"], msgid = "Chapter 1"}
              , PotEntry {references = [], msgid = "Multi\nline \"quoted\" \\ text"}
              ]
        -- WHEN:
        let result = serializePot headers entries
        -- THEN:
        let expected =
              L.intercalate
                "\n"
                [ "msgid \"\""
                , "msgstr \"\""
                , "\"Project-Id-Version: global:core:1.0.0\\n\""
                , "\"Language: en\\n\""
                , ""
                , "#: chapter/5c135d55-1746-4a51-a739-7d495ac4a2ae/title"
                , "msgid \"Chapter 1\""
                , "msgstr \"\""
                , ""
                , "msgid \"Multi\\nline \\\"quoted\\\" \\\\ text\""
                , "msgstr \"\""
                , ""
                ]
        result `shouldBe` expected
      it "round-trips the header through the parser" $ do
        -- GIVEN:
        let headers = [("Language", "cs"), ("Plural-Forms", pluralForms "cs")]
        -- WHEN:
        let result = parsePoHeaderFields (serializePot headers [])
        -- THEN:
        result `shouldBe` Right headers
    describe "parsePoHeaderFields" $ do
      it "parses a realistic PO file with comments and multiline strings" $ do
        -- GIVEN:
        let po =
              L.intercalate
                "\n"
                [ "# Czech translation"
                , "#, fuzzy"
                , "msgid \"\""
                , "msgstr \"\""
                , "\"Project-Id-Version: global:core:1.0.0\\n\""
                , "\"Language: cs\\n\""
                , "\"Content-Type: text/plain; charset=UTF-8\\n\""
                , ""
                , "#: chapter/uuid/title"
                , "msgid \"Chapter 1\""
                , "msgstr \"Kapitola 1\""
                , ""
                , "msgid \"Multi\""
                , "\"line\""
                , "msgstr \"\""
                ]
        -- WHEN:
        let result = parsePoHeaderFields po
        -- THEN:
        fmap (getPoHeaderField "Language") result `shouldBe` Right (Just "cs")
        fmap (getPoHeaderField "Project-Id-Version") result `shouldBe` Right (Just "global:core:1.0.0")
        fmap (getPoHeaderField "Content-Type") result `shouldBe` Right (Just "text/plain; charset=UTF-8")
      it "tolerates plural entries and msgctxt" $ do
        -- GIVEN:
        let po =
              L.intercalate
                "\n"
                [ "msgid \"\""
                , "msgstr \"\""
                , "\"Language: de\\n\""
                , ""
                , "msgctxt \"context\""
                , "msgid \"One item\""
                , "msgid_plural \"More items\""
                , "msgstr[0] \"Ein Element\""
                , "msgstr[1] \"Mehrere Elemente\""
                ]
        -- WHEN:
        let result = parsePoHeaderFields po
        -- THEN:
        fmap (getPoHeaderField "Language") result `shouldBe` Right (Just "de")
      it "fails when the header entry is missing" $ do
        -- GIVEN:
        let po = "msgid \"Chapter 1\"\nmsgstr \"Kapitola 1\"\n"
        -- WHEN:
        let result = parsePoHeaderFields po
        -- THEN:
        result `shouldBe` Left "Missing PO header entry (msgid \"\" with msgstr)"
      it "returns Nothing for a missing or empty Language field" $ do
        -- GIVEN:
        let po = "msgid \"\"\nmsgstr \"\"\n\"Content-Type: text/plain\\n\"\n"
        -- WHEN:
        let result = parsePoHeaderFields po
        -- THEN:
        fmap (getPoHeaderField "Language") result `shouldBe` Right Nothing
    describe "pluralForms" $ do
      it "returns language specific plural forms" $ do
        pluralForms "cs" `shouldBe` "nplurals=3; plural=(n==1) ? 0 : (n>=2 && n<=4) ? 1 : 2;"
        pluralForms "en" `shouldBe` "nplurals=2; plural=(n != 1);"
        pluralForms "ja" `shouldBe` "nplurals=1; plural=0;"
        pluralForms "fr" `shouldBe` "nplurals=2; plural=(n > 1);"
      it "uses the primary subtag and falls back to two plural forms" $ do
        pluralForms "CS-cz" `shouldBe` pluralForms "cs"
        pluralForms "pt_BR" `shouldBe` "nplurals=2; plural=(n != 1);"
        pluralForms "xx" `shouldBe` "nplurals=2; plural=(n != 1);"
