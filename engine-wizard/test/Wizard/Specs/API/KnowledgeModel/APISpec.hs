module Wizard.Specs.API.KnowledgeModel.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.KnowledgeModel.Preview_POST

knowledgeModelAPI appContext =
  with (startWebApp appContext) $ describe "KNOWLEDGE MODEL API Spec" $ preview_post appContext
