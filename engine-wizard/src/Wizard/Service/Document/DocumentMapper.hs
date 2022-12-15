module Wizard.Service.Document.DocumentMapper where

import Data.Time
import qualified Data.UUID as U

import Shared.Model.Common.Lens
import Shared.Model.Template.Template
import Shared.Util.List
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.Submission.SubmissionDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Document.Document
import Wizard.Model.Questionnaire.QuestionnaireEvent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Questionnaire.QuestionnaireSimple
import Wizard.Service.Template.TemplateMapper as Template

toDTO :: Document -> Maybe QuestionnaireSimple -> [SubmissionDTO] -> Template -> DocumentDTO
toDTO doc mQtn submissions tml =
  DocumentDTO
    { uuid = doc.uuid
    , name = doc.name
    , state = doc.state
    , questionnaire = mQtn
    , questionnaireEventUuid = doc.questionnaireEventUuid
    , template = Template.toSimpleDTO tml
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

fromCreateDTO
  :: DocumentCreateDTO -> U.UUID -> DocumentDurability -> Int -> [QuestionnaireEvent] -> Maybe UserDTO -> U.UUID -> UTCTime -> Document
fromCreateDTO dto docUuid durability repliesHash qtnEvents mCurrentUser appUuid now =
  Document
    { uuid = docUuid
    , name = dto.name
    , state = QueuedDocumentState
    , durability = durability
    , questionnaireUuid = dto.questionnaireUuid
    , questionnaireEventUuid =
        case dto.questionnaireEventUuid of
          Just questionnaireEventUuid -> Just questionnaireEventUuid
          Nothing -> fmap getUuid (lastSafe qtnEvents)
    , questionnaireRepliesHash = repliesHash
    , templateId = dto.templateId
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
