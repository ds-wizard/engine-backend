module Wizard.Database.Mapping.DocumentTemplate.DocumentTemplateSuggestion where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow

import Wizard.Database.Mapping.DocumentTemplate.DocumentTemplateState ()
import Wizard.Model.DocumentTemplate.DocumentTemplateSuggestion
import WizardLib.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateJM ()
import WizardLib.DocumentTemplate.Database.Mapping.DocumentTemplate.DocumentTemplatePhase ()
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateJM ()

instance FromRow DocumentTemplateSuggestion where
  fromRow = do
    tId <- field
    name <- field
    organizationId <- field
    templateId <- field
    version <- field
    phase <- field
    metamodelVersion <- field
    description <- field
    allowedPackages <- fieldWith fromJSONField
    formats <- fieldWith fromJSONField
    return $ DocumentTemplateSuggestion {..}
