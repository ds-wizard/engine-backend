module Wizard.Specs.API.BookReference.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.BookReference.Detail_GET
import Wizard.Specs.API.Common

bookReferenceAPI appContext = with (startWebApp appContext) $ describe "BOOK REFERENCE API Spec" $ detail_get appContext
