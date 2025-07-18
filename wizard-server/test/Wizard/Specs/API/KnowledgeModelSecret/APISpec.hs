module Wizard.Specs.API.KnowledgeModelSecret.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.KnowledgeModelSecret.Detail_DELETE
import Wizard.Specs.API.KnowledgeModelSecret.Detail_PUT
import Wizard.Specs.API.KnowledgeModelSecret.List_GET
import Wizard.Specs.API.KnowledgeModelSecret.List_POST

knowledgeModelSecretAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "KNOWLEDGE MODEL SECRET API Spec" $ do
      list_GET appContext
      list_POST appContext
      detail_PUT appContext
      detail_DELETE appContext
