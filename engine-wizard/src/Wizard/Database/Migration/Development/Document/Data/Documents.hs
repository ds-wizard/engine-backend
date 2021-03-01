module Wizard.Database.Migration.Development.Document.Data.Documents where

import Control.Lens ((^.))
import qualified Data.ByteString.Char8 as BS
import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Time
import qualified Data.UUID as U

import LensesConfig hiding (hash)
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.Metric.Data.Metrics
import Shared.Database.Migration.Development.Package.Data.Packages
import Shared.Database.Migration.Development.Template.Data.Templates
import Shared.Model.Common.Lens
import qualified Shared.Service.Package.PackageMapper as SPM
import Wizard.Api.Resource.Document.DocumentContextDTO
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Database.Migration.Development.Config.Data.AppConfigs
import Wizard.Database.Migration.Development.Level.Data.Levels
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Document.Document
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Service.Document.DocumentMapper
import qualified Wizard.Service.Package.PackageMapper as WPM
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionMapper
import qualified Wizard.Service.User.UserMapper as USR_Mapper

doc1 :: Document
doc1 =
  Document
    { _documentUuid = fromJust (U.fromString "264ca352-1a99-4ffd-860e-32aee9a98428")
    , _documentName = "My exported document"
    , _documentState = DoneDocumentState
    , _documentDurability = PersistentDocumentDurability
    , _documentQuestionnaireUuid = questionnaire1 ^. uuid
    , _documentQuestionnaireEventUuid = Just $ last (questionnaire1 ^. events) ^. uuid'
    , _documentQuestionnaireRepliesHash = hash . M.toList $ questionnaire1Ctn ^. replies
    , _documentTemplateId = commonWizardTemplate ^. tId
    , _documentFormatUuid = head (commonWizardTemplate ^. formats) ^. uuid
    , _documentMetadata =
        DocumentMetadata
          {_documentMetadataFileName = Just "export.txt", _documentMetadataContentType = Just "text/plain"}
    , _documentCreatorUuid = Just $ userNikola ^. uuid
    , _documentCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

doc1Content :: BS.ByteString
doc1Content =
  BS.pack $
  "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam auctor pellentesque velit, sollicitudin euismod " ++
  "arcu varius a. Pellentesque consectetur a felis nec finibus. Curabitur at porttitor turpis. Vivamus eu imperdiet " ++
  "massa. Fusce vitae dolor et nulla vulputate condimentum. Aenean tincidunt, magna quis viverra porta, nulla " ++
  "mauris semper nibh, ac interdum quam orci at elit. Donec aliquet tempor erat, sed consectetur sapien eleifend " ++
  "id. Nullam sagittis justo a lobortis fermentum. Nunc pretium sem sed lectus lacinia, et tempus nulla suscipit. " ++
  "Aliquam volutpat molestie nibh sit amet iaculis."

dmp1 :: DocumentContextDTO
dmp1 =
  DocumentContextDTO
    { _documentContextDTOUuid = fromJust (U.fromString "d87941ae-7725-4d22-b5c7-45dabc125199")
    , _documentContextDTOConfig =
        DocumentContextConfigDTO
          {_documentContextConfigDTOLevelsEnabled = True, _documentContextConfigDTOClientUrl = "https://example.com"}
    , _documentContextDTOQuestionnaireUuid = U.toString $ questionnaire1 ^. uuid
    , _documentContextDTOQuestionnaireName = questionnaire1 ^. name
    , _documentContextDTOQuestionnaireReplies = questionnaire1Ctn ^. replies
    , _documentContextDTOQuestionnaireVersion = Nothing
    , _documentContextDTOQuestionnaireVersions = fmap (`toVersionDTO` userAlbert) (questionnaire1 ^. versions)
    , _documentContextDTOLevel = questionnaire1Ctn ^. level
    , _documentContextDTOKnowledgeModel = km1WithQ4
    , _documentContextDTOMetrics = [metricF, metricA, metricI, metricR, metricG, metricO]
    , _documentContextDTOLevels = [level1, level2, level3]
    , _documentContextDTOReport = report1
    , _documentContextDTOPackage = WPM.toSimpleDTO . SPM.toPackage $ germanyPackage
    , _documentContextDTOOrganization = defaultOrganization
    , _documentContextDTOCreatedBy = Just . USR_Mapper.toDTO $ userAlbert
    , _documentContextDTOCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _documentContextDTOUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

doc1Create :: DocumentCreateDTO
doc1Create =
  DocumentCreateDTO
    { _documentCreateDTOName = doc1 ^. name
    , _documentCreateDTOQuestionnaireUuid = doc1 ^. questionnaireUuid
    , _documentCreateDTOQuestionnaireEventUuid = Just $ last (questionnaire1 ^. events) ^. uuid'
    , _documentCreateDTOTemplateId = doc1 ^. templateId
    , _documentCreateDTOFormatUuid = doc1 ^. formatUuid
    }

doc1Dto :: DocumentDTO
doc1Dto = toDTO doc1 (Just questionnaire1Dto) commonWizardTemplate

doc2 :: Document
doc2 =
  Document
    { _documentUuid = fromJust (U.fromString "12de4935-58ad-4a34-9d91-dd0e16619b35")
    , _documentName = "My exported document 2"
    , _documentState = DoneDocumentState
    , _documentDurability = PersistentDocumentDurability
    , _documentQuestionnaireUuid = questionnaire2 ^. uuid
    , _documentQuestionnaireEventUuid = Just $ last (questionnaire2 ^. events) ^. uuid'
    , _documentQuestionnaireRepliesHash = hash . M.toList $ questionnaire2Ctn ^. replies
    , _documentTemplateId = commonWizardTemplate ^. tId
    , _documentFormatUuid = head (commonWizardTemplate ^. formats) ^. uuid
    , _documentMetadata =
        DocumentMetadata
          {_documentMetadataFileName = Just "export.txt", _documentMetadataContentType = Just "text/plain"}
    , _documentCreatorUuid = Just $ userNikola ^. uuid
    , _documentCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

doc3 :: Document
doc3 =
  Document
    { _documentUuid = fromJust (U.fromString "35ef63fd-cb5c-448c-9a4f-54b572573c20")
    , _documentName = "My exported document 3"
    , _documentState = DoneDocumentState
    , _documentDurability = PersistentDocumentDurability
    , _documentQuestionnaireUuid = questionnaire2 ^. uuid
    , _documentQuestionnaireEventUuid = Just $ last (questionnaire2 ^. events) ^. uuid'
    , _documentQuestionnaireRepliesHash = hash . M.toList $ questionnaire2Ctn ^. replies
    , _documentTemplateId = commonWizardTemplate ^. tId
    , _documentFormatUuid = head (commonWizardTemplate ^. formats) ^. uuid
    , _documentMetadata =
        DocumentMetadata
          {_documentMetadataFileName = Just "export.txt", _documentMetadataContentType = Just "text/plain"}
    , _documentCreatorUuid = Just $ userAlbert ^. uuid
    , _documentCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

doc4 :: Document
doc4 =
  doc1
    { _documentUuid = fromJust (U.fromString "9e010fc5-d147-4e9a-94a0-5aba40d78b97")
    , _documentName = "My failed document 4"
    , _documentState = ErrorDocumentState
    }

doc5 :: Document
doc5 =
  doc1
    { _documentUuid = fromJust (U.fromString "c3e1a760-0941-499c-a8cd-6b9d78eee0ba")
    , _documentName = "My in progress document 5"
    , _documentState = InProgressDocumentState
    }

doc6 :: Document
doc6 =
  doc1
    { _documentUuid = fromJust (U.fromString "6a7631bc-af69-4e72-83e4-8440be071005")
    , _documentName = "My queued document 6"
    , _documentState = QueuedDocumentState
    }

tempDocQueued :: Document
tempDocQueued =
  doc1
    { _documentUuid = fromJust (U.fromString "537e2b86-64ec-4ee9-965b-6637775f8f89")
    , _documentName = "My temp docs"
    , _documentState = QueuedDocumentState
    , _documentDurability = TemporallyDocumentDurability
    }

tempDocInProgress :: Document
tempDocInProgress =
  doc1
    { _documentUuid = fromJust (U.fromString "8f075d1e-d7ca-416e-8e17-5dd6d53a01f3")
    , _documentName = "My temp docs"
    , _documentState = InProgressDocumentState
    , _documentDurability = TemporallyDocumentDurability
    }

tempDocDone :: Document
tempDocDone =
  doc1
    { _documentUuid = fromJust (U.fromString "ac38c865-a891-43a7-986b-b6801ed10880")
    , _documentName = "My temp docs"
    , _documentState = DoneDocumentState
    , _documentDurability = TemporallyDocumentDurability
    }

tempDocError :: Document
tempDocError =
  doc1
    { _documentUuid = fromJust (U.fromString "16884341-2771-437d-944f-69bc9572af20")
    , _documentName = "My temp docs"
    , _documentState = ErrorDocumentState
    , _documentDurability = TemporallyDocumentDurability
    }
