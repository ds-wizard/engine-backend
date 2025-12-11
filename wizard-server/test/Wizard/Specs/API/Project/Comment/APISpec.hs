module Wizard.Specs.API.Project.Comment.APISpec where

import Test.Hspec

import Wizard.Specs.API.Project.Comment.List_GET

projectCommentAPI appContext =
  describe "PROJECT COMMENT API Spec" $
    list_GET appContext
