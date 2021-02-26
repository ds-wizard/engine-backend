module Wizard.Service.Document.DocumentMapper where

import Control.Lens ((^.), (^?), _Just)
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Template.Template
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Questionnaire.QuestionnaireDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Document.Document
import Wizard.Service.Template.TemplateMapper as Template

toDTO :: Document -> Maybe QuestionnaireDTO -> Template -> DocumentDTO
toDTO doc mQtn tml =
  DocumentDTO
    { _documentDTOUuid = doc ^. uuid
    , _documentDTOName = doc ^. name
    , _documentDTOState = doc ^. state
    , _documentDTOQuestionnaire = mQtn
    , _documentDTOQuestionnaireEventUuid = doc ^. questionnaireEventUuid
    , _documentDTOTemplate = Template.toSimpleDTO tml
    , _documentDTOFormatUuid = doc ^. formatUuid
    , _documentDTOCreatorUuid = doc ^. creatorUuid
    , _documentDTOCreatedAt = doc ^. createdAt
    }

fromCreateDTO :: DocumentCreateDTO -> U.UUID -> DocumentDurability -> Int -> Maybe UserDTO -> UTCTime -> Document
fromCreateDTO dto docUuid durability repliesHash mCurrentUser now =
  Document
    { _documentUuid = docUuid
    , _documentName = dto ^. name
    , _documentState = QueuedDocumentState
    , _documentDurability = durability
    , _documentQuestionnaireUuid = dto ^. questionnaireUuid
    , _documentQuestionnaireEventUuid = dto ^. questionnaireEventUuid
    , _documentQuestionnaireRepliesHash = repliesHash
    , _documentTemplateId = dto ^. templateId
    , _documentFormatUuid = dto ^. formatUuid
    , _documentMetadata = DocumentMetadata {_documentMetadataFileName = Nothing, _documentMetadataContentType = Nothing}
    , _documentCreatorUuid = mCurrentUser ^? _Just . uuid
    , _documentCreatedAt = now
    }
