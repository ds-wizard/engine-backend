module Wizard.Specs.API.CommentThread.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common

import Wizard.Specs.API.CommentThread.List_GET

commentThreadAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "COMMENT THREAD API Spec" $ do
      list_GET appContext
