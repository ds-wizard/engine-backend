module Wizard.Specs.API.Config.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.Config.List_Affiliation_GET
import Wizard.Specs.API.Config.List_Affiliation_PUT
import Wizard.Specs.API.Config.List_Auth_GET
import Wizard.Specs.API.Config.List_Auth_PUT
import Wizard.Specs.API.Config.List_Bootstrap_GET
import Wizard.Specs.API.Config.List_Client_GET
import Wizard.Specs.API.Config.List_Client_PUT
import Wizard.Specs.API.Config.List_Features_GET
import Wizard.Specs.API.Config.List_Features_PUT
import Wizard.Specs.API.Config.List_Info_GET
import Wizard.Specs.API.Config.List_Info_PUT
import Wizard.Specs.API.Config.List_Organization_GET
import Wizard.Specs.API.Config.List_Organization_PUT

configAPI appContext =
  with (startWebApp appContext) $
  describe "CONFIG API Spec" $ do
    list_bootstrap_GET appContext
    list_affiliation_GET appContext
    list_affiliation_PUT appContext
    list_auth_GET appContext
    list_auth_PUT appContext
    list_client_GET appContext
    list_client_PUT appContext
    list_features_GET appContext
    list_features_PUT appContext
    list_info_GET appContext
    list_info_PUT appContext
    list_organization_GET appContext
    list_organization_PUT appContext
