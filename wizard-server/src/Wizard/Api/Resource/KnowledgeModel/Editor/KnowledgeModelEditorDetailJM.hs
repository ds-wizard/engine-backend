module Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailJM where

import Data.Aeson

import Shared.Common.Util.Aeson
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Event.KnowledgeModelEventJM ()
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorStateJM ()
import Wizard.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleJM ()
import Wizard.Api.Resource.Questionnaire.QuestionnaireReplyJM ()

instance FromJSON KnowledgeModelEditorDetailDTO where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON KnowledgeModelEditorDetailDTO where
  toJSON = genericToJSON jsonOptions
