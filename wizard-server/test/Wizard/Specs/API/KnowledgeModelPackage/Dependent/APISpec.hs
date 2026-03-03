module Wizard.Specs.API.KnowledgeModelPackage.Dependent.APISpec where

import Test.Hspec

import Wizard.Specs.API.KnowledgeModelPackage.Dependent.List_GET

dependentAPI appContext =
  describe "KNOWLEDGE MODEL PACKAGE DEPENDENT API Spec" $ do
    list_GET appContext
