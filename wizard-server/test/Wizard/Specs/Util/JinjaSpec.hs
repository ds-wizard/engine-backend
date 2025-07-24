module Wizard.Specs.Util.JinjaSpec where

import Data.Aeson
import Data.Either (isLeft)
import Test.Hspec

import Wizard.Util.Jinja

jinjaSpec = do
  describe "renderJinjaSingle" $ do
    it "renders a simple template with a single context" $ do
      let template = "Hello {{ name }}!"
          context = object ["name" .= ("Alice" :: String)]
      result <- renderJinjaSingle template context
      result `shouldBe` Right "Hello Alice!"
    it "returns an error for an invalid template" $ do
      let template = "Hello {{ name }"
          context = object ["name" .= ("Alice" :: String)]
      result <- renderJinjaSingle template context
      result `shouldBe` Left "Error preparing Jinja template: unexpected '}'."
    it "handles empty context gracefully" $ do
      let template = "Hello, stranger!"
          context = object []
      result <- renderJinjaSingle template context
      result `shouldBe` Right "Hello, stranger!"
  describe "renderJinjaBatch" $ do
    it "renders multiple templates with different contexts" $ do
      let template = "Hello {{ name }}!"
          contexts =
            [ object ["name" .= ("Alice" :: String)]
            , object ["name" .= ("Bob" :: String)]
            ]
      results <- renderJinjaBatch template contexts
      results `shouldBe` [Right "Hello Alice!", Right "Hello Bob!"]
    it "returns an error for an invalid template" $ do
      let template = "Hello {{ name }"
          contexts =
            [ object ["name" .= ("Alice" :: String)]
            , object ["name" .= ("Bob" :: String)]
            ]
      results <- renderJinjaBatch template contexts
      results `shouldSatisfy` all isLeft
    it "handles empty contexts gracefully" $ do
      let template = "Hello, stranger!"
          contexts = [object [], object []]
      results <- renderJinjaBatch template contexts
      results `shouldBe` [Right "Hello, stranger!", Right "Hello, stranger!"]
    it "handles error on faulty context" $ do
      let template = "Hello {{ item.name }}!"
          contexts =
            [ object ["item" .= object ["name" .= ("Alice" :: String)]]
            , object ["name" .= ("Bob" :: String)]
            ]
      results <- renderJinjaBatch template contexts
      results `shouldSatisfy` (\res -> length res == 2 && isLeft (res !! 1))
  describe "renderJinjaMultiple" $ do
    it "renders two templates with a single context" $ do
      let templates = ["Hello {{ name }}!", "Goodbye {{ name }}!"]
          context = object ["name" .= ("Alice" :: String)]
      result <- renderJinjaMultiple templates context
      result `shouldBe` [Right "Hello Alice!", Right "Goodbye Alice!"]
    it "returns an error for an invalid template" $ do
      let templates = ["Hello {{ name }", "Goodbye {{ name }}!"]
          context = object ["name" .= ("Alice" :: String)]
      result <- renderJinjaMultiple templates context
      result `shouldBe` [Left "Error preparing Jinja template: unexpected '}'.", Right "Goodbye Alice!"]
    it "handles empty context gracefully" $ do
      let templates = ["Hello, stranger!"]
          context = object []
      result <- renderJinjaMultiple templates context
      result `shouldBe` [Right "Hello, stranger!"]
    it "handles disjoint variables in multiple templates" $ do
      let templates = ["Hello {{ name }}!", "Your age is {{ age }}."]
          context = object ["name" .= ("Alice" :: String), "age" .= (30 :: Int)]
      result <- renderJinjaMultiple templates context
      result `shouldBe` [Right "Hello Alice!", Right "Your age is 30."]
