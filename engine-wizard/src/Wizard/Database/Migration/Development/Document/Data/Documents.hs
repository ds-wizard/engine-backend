module Wizard.Database.Migration.Development.Document.Data.Documents where

import qualified Data.ByteString.Char8 as BS
import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Time
import qualified Data.UUID as U

import qualified Shared.Constant.Template as TemplateConstant
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.Model.Template.Template
import qualified Shared.Service.Package.PackageMapper as SPM
import Shared.Util.Uuid
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Database.Migration.Development.App.Data.Apps
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.App.App
import Wizard.Model.Document.Document
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.User.User
import Wizard.Service.Document.DocumentContextMapper
import Wizard.Service.Document.DocumentMapper
import qualified Wizard.Service.Questionnaire.QuestionnaireMapper as QTN_Mapper
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionMapper
import qualified Wizard.Service.User.UserMapper as USR_Mapper

doc1 :: Document
doc1 =
  Document
    { uuid = u' "264ca352-1a99-4ffd-860e-32aee9a98428"
    , name = "My exported document"
    , state = DoneDocumentState
    , durability = PersistentDocumentDurability
    , questionnaireUuid = questionnaire1.uuid
    , questionnaireEventUuid = Just . getUuid . last $ questionnaire1.events
    , questionnaireRepliesHash = hash . M.toList $ questionnaire1Ctn.replies
    , templateId = commonWizardTemplate.tId
    , formatUuid = (head commonWizardTemplate.formats).uuid
    , creatorUuid = Just $ userNikola.uuid
    , fileName = Just "export.txt"
    , contentType = Just "text/plain"
    , fileSize = Just $ 50 * 1024
    , workerLog = Just "Success"
    , appUuid = defaultApp.uuid
    , retrievedAt = Nothing
    , finishedAt = Nothing
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

doc1Content :: BS.ByteString
doc1Content =
  BS.pack $
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam auctor pellentesque velit, sollicitudin euismod "
      ++ "arcu varius a. Pellentesque consectetur a felis nec finibus. Curabitur at porttitor turpis. Vivamus eu imperdiet "
      ++ "massa. Fusce vitae dolor et nulla vulputate condimentum. Aenean tincidunt, magna quis viverra porta, nulla "
      ++ "mauris semper nibh, ac interdum quam orci at elit. Donec aliquet tempor erat, sed consectetur sapien eleifend "
      ++ "id. Nullam sagittis justo a lobortis fermentum. Nunc pretium sem sed lectus lacinia, et tempus nulla suscipit. "
      ++ "Aliquam volutpat molestie nibh sit amet iaculis."

dmp1 :: DocumentContext
dmp1 =
  DocumentContext
    { uuid = u' "d87941ae-7725-4d22-b5c7-45dabc125199"
    , config = DocumentContextConfig {clientUrl = "https://example.com"}
    , questionnaireUuid = U.toString $ questionnaire1.uuid
    , questionnaireName = questionnaire1.name
    , questionnaireDescription = questionnaire1.description
    , questionnaireReplies = questionnaire1Ctn.replies
    , questionnaireVersion = Nothing
    , questionnaireVersions = fmap (`toVersionDTO` Just userAlbert) questionnaire1.versions
    , questionnaireProjectTags = questionnaire1.projectTags
    , phaseUuid = questionnaire1Ctn.phaseUuid
    , knowledgeModel = km1WithQ4
    , report = report1
    , package = toDocumentContextPackage . SPM.toPackage $ germanyPackage
    , organization = defaultOrganization
    , templateMetamodelVersion = TemplateConstant.templateMetamodelVersion
    , createdBy = Just . USR_Mapper.toDTO $ userAlbert
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

doc1Create :: DocumentCreateDTO
doc1Create =
  DocumentCreateDTO
    { name = doc1.name
    , questionnaireUuid = doc1.questionnaireUuid
    , questionnaireEventUuid = Just . getUuid . last $ questionnaire1.events
    , templateId = doc1.templateId
    , formatUuid = doc1.formatUuid
    }

doc1Dto :: DocumentDTO
doc1Dto = toDTO doc1 (Just . QTN_Mapper.toSimple $ questionnaire1) [] commonWizardTemplate

doc2 :: Document
doc2 =
  Document
    { uuid = u' "12de4935-58ad-4a34-9d91-dd0e16619b35"
    , name = "My exported document 2"
    , state = DoneDocumentState
    , durability = PersistentDocumentDurability
    , questionnaireUuid = questionnaire2.uuid
    , questionnaireEventUuid = Just . getUuid . last $ questionnaire2.events
    , questionnaireRepliesHash = hash . M.toList $ questionnaire2Ctn.replies
    , templateId = commonWizardTemplate.tId
    , formatUuid = (head commonWizardTemplate.formats).uuid
    , creatorUuid = Just $ userNikola.uuid
    , fileName = Just "export.txt"
    , contentType = Just "text/plain"
    , fileSize = Just $ 50 * 1024
    , workerLog = Just "Success"
    , appUuid = defaultApp.uuid
    , retrievedAt = Nothing
    , finishedAt = Nothing
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

doc3 :: Document
doc3 =
  Document
    { uuid = u' "35ef63fd-cb5c-448c-9a4f-54b572573c20"
    , name = "My exported document 3"
    , state = DoneDocumentState
    , durability = PersistentDocumentDurability
    , questionnaireUuid = questionnaire2.uuid
    , questionnaireEventUuid = Just . getUuid . last $ questionnaire2.events
    , questionnaireRepliesHash = hash . M.toList $ questionnaire2Ctn.replies
    , templateId = commonWizardTemplate.tId
    , formatUuid = (head commonWizardTemplate.formats).uuid
    , creatorUuid = Just $ userAlbert.uuid
    , fileName = Just "export.txt"
    , contentType = Just "text/plain"
    , fileSize = Just $ 50 * 1024
    , workerLog = Just "Success"
    , appUuid = defaultApp.uuid
    , retrievedAt = Nothing
    , finishedAt = Nothing
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

doc4 :: Document
doc4 =
  doc1
    { uuid = u' "9e010fc5-d147-4e9a-94a0-5aba40d78b97"
    , name = "My failed document 4"
    , state = ErrorDocumentState
    }

doc5 :: Document
doc5 =
  doc1
    { uuid = u' "c3e1a760-0941-499c-a8cd-6b9d78eee0ba"
    , name = "My in progress document 5"
    , state = InProgressDocumentState
    }

doc6 :: Document
doc6 =
  doc1
    { uuid = u' "6a7631bc-af69-4e72-83e4-8440be071005"
    , name = "My queued document 6"
    , state = QueuedDocumentState
    }

tempDocQueued :: Document
tempDocQueued =
  doc1
    { uuid = u' "537e2b86-64ec-4ee9-965b-6637775f8f89"
    , name = "My temp docs"
    , state = QueuedDocumentState
    , durability = TemporallyDocumentDurability
    }

tempDocInProgress :: Document
tempDocInProgress =
  doc1
    { uuid = u' "8f075d1e-d7ca-416e-8e17-5dd6d53a01f3"
    , name = "My temp docs"
    , state = InProgressDocumentState
    , durability = TemporallyDocumentDurability
    }

tempDocDone :: Document
tempDocDone =
  doc1
    { uuid = u' "ac38c865-a891-43a7-986b-b6801ed10880"
    , name = "My temp docs"
    , state = DoneDocumentState
    , durability = TemporallyDocumentDurability
    }

tempDocError :: Document
tempDocError =
  doc1
    { uuid = u' "16884341-2771-437d-944f-69bc9572af20"
    , name = "My temp docs"
    , state = ErrorDocumentState
    , durability = TemporallyDocumentDurability
    }

differentDoc :: Document
differentDoc =
  Document
    { uuid = u' "b9a72acc-8261-4c38-b2d5-bdefea241d59"
    , name = "My different document"
    , state = DoneDocumentState
    , durability = PersistentDocumentDurability
    , questionnaireUuid = differentQuestionnaire.uuid
    , questionnaireEventUuid = Just . getUuid . last $ questionnaire1.events
    , questionnaireRepliesHash = hash . M.toList $ questionnaire1Ctn.replies
    , templateId = anotherWizardTemplate.tId
    , formatUuid = (head anotherWizardTemplate.formats).uuid
    , creatorUuid = Just $ userCharles.uuid
    , fileName = Just "export.txt"
    , contentType = Just "text/plain"
    , fileSize = Just $ 50 * 1024
    , workerLog = Just "Success"
    , appUuid = differentApp.uuid
    , retrievedAt = Nothing
    , finishedAt = Nothing
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }
