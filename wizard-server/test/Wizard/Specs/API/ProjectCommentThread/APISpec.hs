module Wizard.Specs.API.ProjectCommentThread.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common

import Wizard.Specs.API.ProjectCommentThread.List_GET

projectCommentThreadAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "PROJECT COMMENT THREAD API Spec" $ do
      list_GET appContext
