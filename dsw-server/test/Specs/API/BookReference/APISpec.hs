module Specs.API.BookReference.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.API.BookReference.Detail_GET
import Specs.API.Common

bookReferenceAPI appContext = with (startWebApp appContext) $ describe "BOOK REFERENCE API Spec" $ detail_get appContext
