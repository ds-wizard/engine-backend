module Wizard.Specs.API.User.Tour.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.User.Tour.Detail_PUT
import Wizard.Specs.API.User.Tour.List_DELETE

userTourAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "USER TOUR API Spec" $ do
      list_DELETE appContext
      detail_PUT appContext
