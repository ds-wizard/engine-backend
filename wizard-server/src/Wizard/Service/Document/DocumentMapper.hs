module Wizard.Service.Document.DocumentMapper where

import qualified Data.List as L
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Lens
import Shared.Common.Util.JSON
import Shared.Common.Util.List
import Shared.Common.Util.String (trim)
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandMapper
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Document.Document
import Wizard.Model.Document.DocumentContext (DocumentContext)
import Wizard.Model.Document.DocumentContextJM ()
import Wizard.Model.Document.DocumentList
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Questionnaire.QuestionnaireSimple
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateMapper

toDTO :: DocumentList -> [SubmissionDTO] -> DocumentDTO
toDTO doc submissions =
  DocumentDTO
    { uuid = doc.uuid
    , name = doc.name
    , state = doc.state
    , questionnaire = Just QuestionnaireSimple {uuid = doc.questionnaireUuid, name = doc.questionnaireName}
    , questionnaireEventUuid = doc.questionnaireEventUuid
    , documentTemplateName = doc.documentTemplateName
    , format = fmap toFormatDTO . L.find (\f -> f.uuid == doc.formatUuid) $ doc.documentTemplateFormats
    , fileSize = doc.fileSize
    , workerLog =
        case doc.state of
          ErrorDocumentState -> doc.workerLog
          _ -> Nothing
    , submissions = submissions
    , createdBy = doc.createdBy
    , createdAt = doc.createdAt
    }

toDTOWithDocTemplate :: Document -> Maybe QuestionnaireSimple -> [SubmissionDTO] -> DocumentTemplate -> DocumentDTO
toDTOWithDocTemplate doc mQtn submissions tml =
  DocumentDTO
    { uuid = doc.uuid
    , name = doc.name
    , state = doc.state
    , questionnaire = mQtn
    , questionnaireEventUuid = doc.questionnaireEventUuid
    , documentTemplateName = tml.name
    , format = fmap toFormatDTO . L.find (\f -> f.uuid == doc.formatUuid) $ tml.formats
    , fileSize = doc.fileSize
    , workerLog =
        case doc.state of
          ErrorDocumentState -> doc.workerLog
          _ -> Nothing
    , submissions = submissions
    , createdBy = doc.createdBy
    , createdAt = doc.createdAt
    }

fromCreateDTO :: DocumentCreateDTO -> U.UUID -> Int -> [QuestionnaireEvent] -> Maybe UserDTO -> U.UUID -> UTCTime -> Document
fromCreateDTO dto docUuid repliesHash qtnEvents mCurrentUser tenantUuid now =
  Document
    { uuid = docUuid
    , name = trim dto.name
    , state = QueuedDocumentState
    , durability = PersistentDocumentDurability
    , questionnaireUuid = dto.questionnaireUuid
    , questionnaireEventUuid =
        case dto.questionnaireEventUuid of
          Just questionnaireEventUuid -> Just questionnaireEventUuid
          Nothing -> fmap getUuid (lastSafe qtnEvents)
    , questionnaireRepliesHash = repliesHash
    , documentTemplateId = dto.documentTemplateId
    , formatUuid = dto.formatUuid
    , createdBy = fmap (.uuid) mCurrentUser
    , fileName = Nothing
    , contentType = Nothing
    , fileSize = Nothing
    , workerLog = Nothing
    , tenantUuid = tenantUuid
    , retrievedAt = Nothing
    , finishedAt = Nothing
    , createdAt = now
    }

fromTemporallyCreateDTO :: U.UUID -> Questionnaire -> String -> U.UUID -> Int -> Maybe UserDTO -> U.UUID -> UTCTime -> Document
fromTemporallyCreateDTO docUuid qtn documentTemplateId formatUuid repliesHash mCurrentUser tenantUuid now =
  Document
    { uuid = docUuid
    , name = trim qtn.name
    , state = QueuedDocumentState
    , durability = TemporallyDocumentDurability
    , questionnaireUuid = qtn.uuid
    , questionnaireEventUuid = fmap getUuid (lastSafe qtn.events)
    , questionnaireRepliesHash = repliesHash
    , documentTemplateId = documentTemplateId
    , formatUuid = formatUuid
    , createdBy = fmap (.uuid) mCurrentUser
    , fileName = Nothing
    , contentType = Nothing
    , fileSize = Nothing
    , workerLog = Nothing
    , tenantUuid = tenantUuid
    , retrievedAt = Nothing
    , finishedAt = Nothing
    , createdAt = now
    }

toDocPersistentCommand :: U.UUID -> DocumentContext -> Document -> PersistentCommand U.UUID
toDocPersistentCommand uuid docContext doc =
  toPersistentCommand
    uuid
    "doc_worker"
    "generateDocument"
    (encodeJsonToString docContext)
    10
    False
    Nothing
    doc.tenantUuid
    doc.createdBy
    doc.createdAt
