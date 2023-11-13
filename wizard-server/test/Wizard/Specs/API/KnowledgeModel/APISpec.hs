module Wizard.Specs.API.KnowledgeModel.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.KnowledgeModel.Preview_POST

knowledgeModelAPI baseContext appContext =
  with (startWebApp baseContext appContext) $ describe "KNOWLEDGE MODEL API Spec" $ preview_POST appContext
