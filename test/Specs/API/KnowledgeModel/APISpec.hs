module Specs.API.KnowledgeModel.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.API.Common
import Specs.API.KnowledgeModel.Preview_POST

knowledgeModelAPI appContext =
  with (startWebApp appContext) $ describe "KNOWLEDGE MODEL API Spec" $ preview_post appContext
