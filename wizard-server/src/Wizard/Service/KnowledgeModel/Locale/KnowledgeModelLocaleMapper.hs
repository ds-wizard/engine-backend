module Wizard.Service.KnowledgeModel.Locale.KnowledgeModelLocaleMapper where

import Wizard.Model.KnowledgeModel.Locale.KnowledgeModelLocale
import Wizard.Model.KnowledgeModel.Locale.KnowledgeModelLocaleList

toList :: KnowledgeModelLocale -> KnowledgeModelLocaleList
toList locale =
  KnowledgeModelLocaleList
    { uuid = locale.uuid
    , name = locale.name
    , code = locale.code
    }
