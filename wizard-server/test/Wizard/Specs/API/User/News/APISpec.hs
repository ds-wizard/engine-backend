module Wizard.Specs.API.User.News.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.User.News.Detail_PUT

userNewsAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "USER NEWS API Spec" $
      detail_PUT appContext
