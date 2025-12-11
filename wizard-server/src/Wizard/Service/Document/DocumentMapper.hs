module Wizard.Service.Document.DocumentMapper where

import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Model.Common.Lens
import Shared.Common.Util.JSON
import Shared.Common.Util.List
import Shared.Common.Util.String (trim)
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplateFormatSimple
import Shared.KnowledgeModel.Constant.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Shared.PersistentCommand.Model.PersistentCommand.PersistentCommand
import Shared.PersistentCommand.Service.PersistentCommand.PersistentCommandMapper
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Api.Resource.User.UserDTO
import Wizard.Model.Document.Document
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Document.DocumentContextJM ()
import Wizard.Model.Document.DocumentList
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.Project.Event.ProjectEventList
import Wizard.Model.Project.Event.ProjectEventListLenses ()
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectSimple
import Wizard.Model.Submission.SubmissionList

toDTO :: DocumentList -> [SubmissionList] -> DocumentDTO
toDTO doc submissions =
  DocumentDTO
    { uuid = doc.uuid
    , name = doc.name
    , state = doc.state
    , project = Just ProjectSimple {uuid = doc.projectUuid, name = doc.projectName}
    , projectEventUuid = doc.projectEventUuid
    , projectVersion = doc.projectVersion
    , documentTemplateId = doc.documentTemplateId
    , documentTemplateName = doc.documentTemplateName
    , format = L.find (\f -> f.uuid == doc.formatUuid) $ doc.documentTemplateFormats
    , fileSize = doc.fileSize
    , workerLog =
        case doc.state of
          ErrorDocumentState -> doc.workerLog
          _ -> Nothing
    , submissions = submissions
    , createdBy = doc.createdBy
    , createdAt = doc.createdAt
    }

toDTOWithDocTemplate :: Document -> Project -> Maybe String -> [SubmissionList] -> DocumentTemplate -> DocumentTemplateFormatSimple -> DocumentDTO
toDTOWithDocTemplate doc project mProjectVersion submissions tml format =
  DocumentDTO
    { uuid = doc.uuid
    , name = doc.name
    , state = doc.state
    , project = Just $ ProjectSimple {uuid = project.uuid, name = project.name}
    , projectEventUuid = doc.projectEventUuid
    , projectVersion = mProjectVersion
    , documentTemplateId = tml.tId
    , documentTemplateName = tml.name
    , format = Just format
    , fileSize = doc.fileSize
    , workerLog =
        case doc.state of
          ErrorDocumentState -> doc.workerLog
          _ -> Nothing
    , submissions = submissions
    , createdBy = doc.createdBy
    , createdAt = doc.createdAt
    }

fromCreateDTO :: DocumentCreateDTO -> U.UUID -> Int -> [ProjectEventList] -> Maybe UserDTO -> U.UUID -> UTCTime -> Document
fromCreateDTO dto docUuid repliesHash projectEvents mCurrentUser tenantUuid now =
  Document
    { uuid = docUuid
    , name = trim dto.name
    , state = QueuedDocumentState
    , durability = PersistentDocumentDurability
    , projectUuid = Just dto.projectUuid
    , projectEventUuid =
        case dto.projectEventUuid of
          Just projectEventUuid -> Just projectEventUuid
          Nothing -> fmap getUuid (lastSafe projectEvents)
    , projectRepliesHash = repliesHash
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

fromTemporallyCreateDTO :: U.UUID -> Project -> Maybe U.UUID -> String -> U.UUID -> Int -> Maybe UserDTO -> U.UUID -> UTCTime -> Bool -> Document
fromTemporallyCreateDTO docUuid project projectEventUuid documentTemplateId formatUuid repliesHash mCurrentUser tenantUuid now fromKnowledgeModelEditor =
  Document
    { uuid = docUuid
    , name = trim project.name
    , state = QueuedDocumentState
    , durability = TemporallyDocumentDurability
    , projectUuid =
        if fromKnowledgeModelEditor
          then Nothing
          else Just project.uuid
    , projectEventUuid = projectEventUuid
    , projectRepliesHash = repliesHash
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

toTemporaryPackage :: U.UUID -> UTCTime -> KnowledgeModelPackage
toTemporaryPackage tenantUuid createdAt =
  KnowledgeModelPackage
    { pId = "org.example:km-example:1.0.0"
    , name = "Example Knowledge Model"
    , organizationId = "org.example"
    , kmId = "km-example"
    , version = "1.0.0"
    , phase = ReleasedKnowledgeModelPackagePhase
    , metamodelVersion = knowledgeModelMetamodelVersion
    , description = "Example description"
    , readme = "# Example Knowledge Model\n\nThis is an example knowledge model."
    , license = "Apache-2.0"
    , previousPackageId = Nothing
    , forkOfPackageId = Nothing
    , mergeCheckpointPackageId = Nothing
    , nonEditable = False
    , tenantUuid = tenantUuid
    , createdAt = createdAt
    }

toTemporaryProject :: KnowledgeModelEditor -> KnowledgeModelPackage -> Maybe UserDTO -> Project
toTemporaryProject kmEditor package mCurrentUser =
  Project
    { uuid = kmEditor.uuid
    , name = kmEditor.name
    , description = Just kmEditor.description
    , visibility = PrivateProjectVisibility
    , sharing = RestrictedProjectSharing
    , knowledgeModelPackageId = fromMaybe package.pId kmEditor.previousPackageId
    , selectedQuestionTagUuids = []
    , projectTags = []
    , documentTemplateId = Nothing
    , formatUuid = Nothing
    , creatorUuid = fmap (.uuid) mCurrentUser
    , permissions = []
    , isTemplate = False
    , squashed = True
    , tenantUuid = kmEditor.tenantUuid
    , createdAt = kmEditor.createdAt
    , updatedAt = kmEditor.updatedAt
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
