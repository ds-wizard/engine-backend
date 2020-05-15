module Wizard.Service.Document.DocumentMapper where

import Control.Lens ((^.))
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Model.Document.Document
import Wizard.Model.Template.Template

toDTO :: Document -> Maybe QuestionnaireDTO -> Template -> DocumentDTO
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

fromCreateDTO :: DocumentCreateDTO -> U.UUID -> DocumentDurability -> Int -> U.UUID -> UTCTime -> Document
fromCreateDTO dto uuid durability repliesHash currentUserUuid now =
  Document
    { _documentUuid = uuid
    , _documentName = dto ^. name
    , _documentState = QueuedDocumentState
    , _documentDurability = durability
    , _documentQuestionnaireUuid = dto ^. questionnaireUuid
    , _documentQuestionnaireRepliesHash = repliesHash
    , _documentTemplateUuid = dto ^. templateUuid
    , _documentFormatUuid = dto ^. formatUuid
    , _documentMetadata = DocumentMetadata {_documentMetadataFileName = Nothing, _documentMetadataContentType = Nothing}
    , _documentOwnerUuid = currentUserUuid
    , _documentCreatedAt = now
    }
