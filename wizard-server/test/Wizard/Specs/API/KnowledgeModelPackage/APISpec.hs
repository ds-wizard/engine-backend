module Wizard.Specs.API.KnowledgeModelPackage.APISpec where

import Test.Hspec
import Test.Hspec.Wai hiding (shouldRespondWith)

import Wizard.Specs.API.Common
import Wizard.Specs.API.KnowledgeModelPackage.Dependent.APISpec
import Wizard.Specs.API.KnowledgeModelPackage.Detail_DELETE
import Wizard.Specs.API.KnowledgeModelPackage.Detail_GET
import Wizard.Specs.API.KnowledgeModelPackage.Detail_Locales_Content_GET
import Wizard.Specs.API.KnowledgeModelPackage.Detail_Locales_DELETE
import Wizard.Specs.API.KnowledgeModelPackage.Detail_Locales_GET
import Wizard.Specs.API.KnowledgeModelPackage.Detail_Locales_POST
import Wizard.Specs.API.KnowledgeModelPackage.Detail_Locales_Template_GET
import Wizard.Specs.API.KnowledgeModelPackage.Detail_PUT
import Wizard.Specs.API.KnowledgeModelPackage.Detail_Pull_POST
import Wizard.Specs.API.KnowledgeModelPackage.List_From_Editor_POST
import Wizard.Specs.API.KnowledgeModelPackage.List_GET
import Wizard.Specs.API.KnowledgeModelPackage.List_POST
import Wizard.Specs.API.KnowledgeModelPackage.List_Suggestions_GET

knowledgeModelPackageAPI baseContext appContext =
  with (startWebApp baseContext appContext) $
    describe "KNOWLEDGE MODEL PACKAGE API Spec" $ do
      list_GET appContext
      list_suggestions_GET appContext
      list_POST appContext
      list_from_editor_POST appContext
      detail_GET appContext
      detail_PUT appContext
      detail_DELETE appContext
      detail_pull_POST appContext
      detail_locales_GET appContext
      detail_locales_POST appContext
      detail_locales_template_GET appContext
      detail_locales_content_GET appContext
      detail_locales_DELETE appContext
      dependentAPI appContext
