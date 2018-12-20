module Specs.API.KnowledgeModel.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Specs.API.Common
import Specs.API.KnowledgeModel.Detail_GET

knowledgeModelAPI appContext =
  with (startWebApp appContext) $ describe "KNOWLEDGE MODEL API Spec" $ detail_get appContext
