module Wizard.Service.Document.DocumentMapper where

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
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Questionnaire.QuestionnaireSimple
import Wizard.Service.DocumentTemplate.DocumentTemplateMapper as DocumentTemplate
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

toDTO :: Document -> Maybe QuestionnaireSimple -> [SubmissionDTO] -> DocumentTemplate -> DocumentDTO
toDTO doc mQtn submissions tml =
  DocumentDTO
    { uuid = doc.uuid
    , name = doc.name
    , state = doc.state
    , questionnaire = mQtn
    , questionnaireEventUuid = doc.questionnaireEventUuid
    , documentTemplate = DocumentTemplate.toSimpleDTO tml
    , formatUuid = doc.formatUuid
    , fileName = doc.fileName
    , contentType = doc.contentType
    , fileSize = doc.fileSize
    , workerLog =
        case doc.state of
          ErrorDocumentState -> doc.workerLog
          _ -> Nothing
    , submissions = submissions
    , creatorUuid = doc.creatorUuid
    , createdAt = doc.createdAt
    }

fromCreateDTO :: DocumentCreateDTO -> U.UUID -> Int -> [QuestionnaireEvent] -> Maybe UserDTO -> U.UUID -> UTCTime -> Document
fromCreateDTO dto docUuid repliesHash qtnEvents mCurrentUser appUuid now =
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
    , creatorUuid = fmap (.uuid) mCurrentUser
    , fileName = Nothing
    , contentType = Nothing
    , fileSize = Nothing
    , workerLog = Nothing
    , appUuid = appUuid
    , retrievedAt = Nothing
    , finishedAt = Nothing
    , createdAt = now
    }

fromTemporallyCreateDTO
  :: U.UUID -> Questionnaire -> String -> U.UUID -> Int -> Maybe UserDTO -> U.UUID -> UTCTime -> Document
fromTemporallyCreateDTO docUuid qtn documentTemplateId formatUuid repliesHash mCurrentUser appUuid now =
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
    , creatorUuid = fmap (.uuid) mCurrentUser
    , fileName = Nothing
    , contentType = Nothing
    , fileSize = Nothing
    , workerLog = Nothing
    , appUuid = appUuid
    , retrievedAt = Nothing
    , finishedAt = Nothing
    , createdAt = now
    }

toDocPersistentCommand :: U.UUID -> DocumentContext -> Document -> PersistentCommand U.UUID
toDocPersistentCommand pUuid docContext doc =
  toPersistentCommand
    pUuid
    "doc_worker"
    "generateDocument"
    (encodeJsonToString docContext)
    10
    False
    doc.appUuid
    doc.creatorUuid
    doc.createdAt
