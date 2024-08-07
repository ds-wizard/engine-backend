module Wizard.Database.Migration.Development.Document.Data.Documents where

import qualified Data.ByteString.Char8 as BS
import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Time

import Shared.Common.Util.Uuid
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Database.Migration.Development.Questionnaire.Data.Questionnaires
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Document.Document
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Questionnaire.Questionnaire
import Wizard.Model.Questionnaire.QuestionnaireContent
import Wizard.Model.Questionnaire.QuestionnaireEventLenses ()
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.User
import Wizard.Service.Document.Context.DocumentContextMapper
import Wizard.Service.Document.DocumentMapper
import qualified Wizard.Service.Questionnaire.QuestionnaireMapper as QTN_Mapper
import Wizard.Service.Questionnaire.Version.QuestionnaireVersionMapper
import qualified Wizard.Service.User.UserMapper as USR_Mapper
import qualified WizardLib.DocumentTemplate.Constant.DocumentTemplate as TemplateConstant
import WizardLib.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import WizardLib.KnowledgeModel.Database.Migration.Development.Package.Data.Packages
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses
import qualified WizardLib.KnowledgeModel.Service.Package.PackageMapper as SPM

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
    , documentTemplateId = wizardDocumentTemplate.tId
    , formatUuid = (head wizardDocumentTemplate.formats).uuid
    , createdBy = Just $ userNikola.uuid
    , fileName = Just "export.txt"
    , contentType = Just "text/plain"
    , fileSize = Just $ 50 * 1024
    , workerLog = Just "Success"
    , tenantUuid = defaultTenant.uuid
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
    { config = DocumentContextConfig {clientUrl = "https://example.com/wizard"}
    , document =
        DocumentContextDocument
          { uuid = doc1.uuid
          , name = doc1.name
          , documentTemplateId = doc1.documentTemplateId
          , formatUuid = doc1.formatUuid
          , createdBy = Just . USR_Mapper.toDTO $ userNikola
          , createdAt = doc1.createdAt
          }
    , questionnaire =
        DocumentContextQuestionnaire
          { uuid = questionnaire1.uuid
          , name = questionnaire1.name
          , description = questionnaire1.description
          , replies = questionnaire1Ctn.replies
          , phaseUuid = questionnaire1Ctn.phaseUuid
          , labels = questionnaire1Ctn.labels
          , versionUuid = Nothing
          , versions = fmap (`toVersionDTO` Just userAlbert) questionnaire1.versions
          , projectTags = questionnaire1.projectTags
          , createdBy = Just . USR_Mapper.toDTO $ userAlbert
          , createdAt = questionnaire1.createdAt
          , updatedAt = questionnaire1.updatedAt
          }
    , knowledgeModel = km1WithQ4
    , report = report1
    , package = toDocumentContextPackage . SPM.toPackage $ germanyPackage
    , organization = defaultOrganization
    , metamodelVersion = TemplateConstant.documentTemplateMetamodelVersion
    , users =
        [ DocumentContextUserPerm
            { user = USR_Mapper.toDTO userAlbert
            , perms = ["VIEW", "COMMENT", "EDIT", "ADMIN"]
            }
        ]
    , groups = []
    }

doc1Create :: DocumentCreateDTO
doc1Create =
  DocumentCreateDTO
    { name = doc1.name
    , questionnaireUuid = doc1.questionnaireUuid
    , questionnaireEventUuid = Just . getUuid . last $ questionnaire1.events
    , documentTemplateId = doc1.documentTemplateId
    , formatUuid = doc1.formatUuid
    }

doc1Dto :: DocumentDTO
doc1Dto = toDTOWithDocTemplate doc1 (Just . QTN_Mapper.toSimple $ questionnaire1) Nothing [] wizardDocumentTemplate

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
    , documentTemplateId = wizardDocumentTemplate.tId
    , formatUuid = (head wizardDocumentTemplate.formats).uuid
    , createdBy = Just $ userNikola.uuid
    , fileName = Just "export.txt"
    , contentType = Just "text/plain"
    , fileSize = Just $ 50 * 1024
    , workerLog = Just "Success"
    , tenantUuid = defaultTenant.uuid
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
    , documentTemplateId = wizardDocumentTemplate.tId
    , formatUuid = (head wizardDocumentTemplate.formats).uuid
    , createdBy = Just $ userAlbert.uuid
    , fileName = Just "export.txt"
    , contentType = Just "text/plain"
    , fileSize = Just $ 50 * 1024
    , workerLog = Just "Success"
    , tenantUuid = defaultTenant.uuid
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
    , documentTemplateId = anotherWizardDocumentTemplate.tId
    , formatUuid = (head anotherWizardDocumentTemplate.formats).uuid
    , createdBy = Just $ userCharles.uuid
    , fileName = Just "export.txt"
    , contentType = Just "text/plain"
    , fileSize = Just $ 50 * 1024
    , workerLog = Just "Success"
    , tenantUuid = differentTenant.uuid
    , retrievedAt = Nothing
    , finishedAt = Nothing
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 20) 0
    }
