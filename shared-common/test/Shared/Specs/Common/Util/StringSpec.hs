module Shared.Specs.Common.Util.StringSpec where

import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

import Shared.Common.Util.String

stringSpec =
  describe "StringSpec" $ do
    describe "f'" $ do
      createTestF' "My String" [] "My String"
      createTestF' "My %s String" ["Super"] "My Super String"
      createTestF' "My String %s" ["Super"] "My String Super"
      createTestF' "%s My String" ["Super"] "Super My String"
      createTestF' "My %%s String" ["Super"] "My %s String"
      createTestF' "My %s String" [] "My %s String"
      createTestF' "My %s String %s %s" ["Super", "abc", "123"] "My Super String abc 123"
      createTestF' "My %s String %s %s" ["Super", "abc"] "My Super String abc %s"
    describe "f''" $ do
      createTestF'' "My String" [] "My String"
      createTestF'' "My ${myVar} String" [("myVar", "Super")] "My Super String"
      createTestF'' "My String ${myVar}" [("myVar", "Super")] "My String Super"
      createTestF'' "${myVar} My String" [("myVar", "Super")] "Super My String"
      createTestF'' "My ${myVar} String ${myVar}" [("myVar", "Super")] "My Super String Super"
      createTestF'' "My $${myVar} String" [("myVar", "Super")] "My $Super String"
      createTestF'' "My ${myVar} String" [] "My ${myVar} String"
      createTestF'' "My ${myVar} String ${anotherVar} ${number}" [("myVar", "Super"), ("anotherVar", "abc"), ("number", "123")] "My Super String abc 123"
      createTestF'' "My ${myVar} String ${anotherVar} ${number}" [("myVar", "Super"), ("anotherVar", "abc")] "My Super String abc ${number}"

createTestF' template variables expected =
  it ("Expand '" ++ template ++ "' with variables " ++ show variables ++ " to '" ++ expected ++ "'") $
    f' template variables `shouldBe` expected

createTestF'' template variables expected =
  it ("Expand '" ++ template ++ "' with variables " ++ show variables ++ " to '" ++ expected ++ "'") $
    f'' template variables `shouldBe` expected
