module Wizard.Database.Migration.Development.Document.Data.Documents where

import Control.Lens ((^.))
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Time
import qualified Data.UUID as U

import LensesConfig
import Shared.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.Database.Migration.Development.Metric.Data.Metrics
import Wizard.Database.Migration.Development.Level.Data.Levels
import Wizard.Database.Migration.Development.Organization.Data.Organizations
import Wizard.Database.Migration.Development.Package.Data.Packages
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Database.Migration.Development.Template.Data.Templates
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Document.Document
import Wizard.Model.Document.DocumentContext
import Wizard.Service.Package.PackageMapper

doc1 :: Document
doc1 =
  Document
    { _documentUuid = fromJust (U.fromString "264ca352-1a99-4ffd-860e-32aee9a98428")
    , _documentName = "My exported document"
    , _documentState = DoneDocumentState
    , _documentQuestionnaireUuid = questionnaire1 ^. uuid
    , _documentTemplateUuid = commonWizardTemplate ^. uuid
    , _documentFormatUuid = head (commonWizardTemplate ^. formats) ^. uuid
    , _documentMetadata =
        DocumentMetadata
          {_documentMetadataFileName = Just "export.txt", _documentMetadataContentType = Just "text/plain"}
    , _documentOwnerUuid = userNikola ^. uuid
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

dmp1 :: DocumentContext
dmp1 =
  DocumentContext
    { _documentContextUuid = fromJust (U.fromString "d87941ae-7725-4d22-b5c7-45dabc125199")
    , _documentContextConfig = DocumentContextConfig {_documentContextConfigLevelsEnabled = True}
    , _documentContextQuestionnaireUuid = U.toString $ questionnaire1 ^. uuid
    , _documentContextQuestionnaireName = questionnaire1 ^. name
    , _documentContextQuestionnaireReplies = questionnaire1 ^. replies
    , _documentContextLevel = questionnaire1 ^. level
    , _documentContextKnowledgeModel = km1WithQ4
    , _documentContextMetrics = [metricF, metricA, metricI, metricR, metricG, metricO]
    , _documentContextLevels = [level1, level2, level3]
    , _documentContextReport = report1
    , _documentContextPackage = toPackage germanyPackage
    , _documentContextOrganization = org1
    , _documentContextCreatedBy = Just userAlbert
    , _documentContextCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    , _documentContextUpdatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

doc2 :: Document
doc2 =
  Document
    { _documentUuid = fromJust (U.fromString "12de4935-58ad-4a34-9d91-dd0e16619b35")
    , _documentName = "My exported document 2"
    , _documentState = DoneDocumentState
    , _documentQuestionnaireUuid = questionnaire2 ^. uuid
    , _documentTemplateUuid = commonWizardTemplate ^. uuid
    , _documentFormatUuid = head (commonWizardTemplate ^. formats) ^. uuid
    , _documentMetadata =
        DocumentMetadata
          {_documentMetadataFileName = Just "export.txt", _documentMetadataContentType = Just "text/plain"}
    , _documentOwnerUuid = userNikola ^. uuid
    , _documentCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }

doc3 :: Document
doc3 =
  Document
    { _documentUuid = fromJust (U.fromString "35ef63fd-cb5c-448c-9a4f-54b572573c20")
    , _documentName = "My exported document 3"
    , _documentState = DoneDocumentState
    , _documentQuestionnaireUuid = questionnaire2 ^. uuid
    , _documentTemplateUuid = commonWizardTemplate ^. uuid
    , _documentFormatUuid = head (commonWizardTemplate ^. formats) ^. uuid
    , _documentMetadata =
        DocumentMetadata
          {_documentMetadataFileName = Just "export.txt", _documentMetadataContentType = Just "text/plain"}
    , _documentOwnerUuid = userAlbert ^. uuid
    , _documentCreatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }
