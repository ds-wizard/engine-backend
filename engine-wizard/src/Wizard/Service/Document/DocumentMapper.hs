module Wizard.Service.Document.DocumentMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.Template.TemplateDTO
import Wizard.Model.Document.Document

toDTO :: Document -> Maybe QuestionnaireDTO -> TemplateDTO -> DocumentDTO
toDTO doc mQtn tml =
  DocumentDTO
    { _documentDTOUuid = doc ^. uuid
    , _documentDTOName = doc ^. name
    , _documentDTOState = doc ^. state
    , _documentDTOQuestionnaire = mQtn
    , _documentDTOTemplate = tml
    , _documentDTOFormatUuid = doc ^. formatUuid
    , _documentDTOOwnerUuid = doc ^. ownerUuid
    , _documentDTOCreatedAt = doc ^. createdAt
    }

fromCreateDTO :: DocumentCreateDTO -> U.UUID -> U.UUID -> UTCTime -> Document
fromCreateDTO dto uuid currentUserUuid now =
  Document
    { _documentUuid = uuid
    , _documentName = dto ^. name
    , _documentState = QueuedDocumentState
    , _documentQuestionnaireUuid = dto ^. questionnaireUuid
    , _documentTemplateUuid = dto ^. templateUuid
    , _documentFormatUuid = dto ^. formatUuid
    , _documentMetadata = DocumentMetadata {_documentMetadataFileName = Nothing, _documentMetadataContentType = Nothing}
    , _documentOwnerUuid = currentUserUuid
    , _documentCreatedAt = now
    }
