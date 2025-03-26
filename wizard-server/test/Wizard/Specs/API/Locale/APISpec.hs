module Wizard.Specs.API.Locale.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common

import Wizard.Specs.API.Locale.Detail_DELETE
import Wizard.Specs.API.Locale.Detail_GET
import Wizard.Specs.API.Locale.Detail_PUT
import Wizard.Specs.API.Locale.List_Current_Content_GET
import Wizard.Specs.API.Locale.List_DELETE
import Wizard.Specs.API.Locale.List_GET
import Wizard.Specs.API.Locale.List_Suggestions_GET

localeAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "LOCALE API Spec" $ do
      list_GET appContext
      list_suggestions_GET appContext
      list_current_content_GET appContext
      list_DELETE appContext
      detail_GET appContext
      detail_PUT appContext
      detail_DELETE appContext
