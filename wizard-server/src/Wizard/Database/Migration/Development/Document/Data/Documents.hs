module Wizard.Database.Migration.Development.Document.Data.Documents where

import qualified Data.ByteString.Char8 as BS
import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Time

import Shared.Common.Util.Uuid
import qualified Shared.DocumentTemplate.Constant.DocumentTemplate as TemplateConstant
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplateFormats
import Shared.DocumentTemplate.Database.Migration.Development.DocumentTemplate.Data.DocumentTemplates
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Api.Resource.Document.DocumentCreateDTO
import Wizard.Api.Resource.Document.DocumentDTO
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Database.Migration.Development.Report.Data.Reports
import Wizard.Database.Migration.Development.Tenant.Data.TenantConfigs
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.Document.Document
import Wizard.Model.Document.DocumentContext
import Wizard.Model.Project.Project
import Wizard.Model.Project.ProjectContent
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.User
import Wizard.Service.Document.Context.DocumentContextMapper
import Wizard.Service.Document.DocumentMapper
import Wizard.Service.Project.Version.ProjectVersionMapper
import qualified Wizard.Service.User.UserMapper as USR_Mapper

doc1 :: Document
doc1 =
  Document
    { uuid = u' "264ca352-1a99-4ffd-860e-32aee9a98428"
    , name = "My exported document"
    , state = DoneDocumentState
    , durability = PersistentDocumentDurability
    , projectUuid = Just project1.uuid
    , projectEventUuid = Just . getUuid . last $ project1Events
    , projectRepliesHash = hash . M.toList $ project1Ctn.replies
    , documentTemplateId = wizardDocumentTemplate.tId
    , formatUuid = formatJson.uuid
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
    { config =
        DocumentContextConfig
          { clientUrl = "https://example.com/wizard"
          , appTitle = Nothing
          , appTitleShort = Nothing
          , illustrationsColor = Nothing
          , primaryColor = Nothing
          , logoUrl = Nothing
          }
    , document =
        DocumentContextDocument
          { uuid = doc1.uuid
          , name = doc1.name
          , documentTemplateId = doc1.documentTemplateId
          , formatUuid = doc1.formatUuid
          , createdBy = Just . USR_Mapper.toDTO $ userNikola
          , createdAt = doc1.createdAt
          }
    , project =
        DocumentContextQuestionnaire
          { uuid = project1.uuid
          , name = project1.name
          , description = project1.description
          , replies = project1Ctn.replies
          , phaseUuid = project1Ctn.phaseUuid
          , labels = project1Ctn.labels
          , versionUuid = Nothing
          , versions = fmap (`toVersionList` Just userAlbertDto) project1Versions
          , projectTags = project1.projectTags
          , files = []
          , createdBy = Just . USR_Mapper.toDTO $ userAlbert
          , createdAt = project1.createdAt
          , updatedAt = project1.updatedAt
          }
    , knowledgeModel = km1WithQ4
    , report = report1
    , package = toDocumentContextPackage germanyKmPackage
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
    , projectUuid = project1.uuid
    , projectEventUuid = Just . getUuid . last $ project1Events
    , documentTemplateId = doc1.documentTemplateId
    , formatUuid = doc1.formatUuid
    }

doc1Dto :: DocumentDTO
doc1Dto = toDTOWithDocTemplate doc1 project1 Nothing [] wizardDocumentTemplate formatJsonSimple

doc2 :: Document
doc2 =
  Document
    { uuid = u' "12de4935-58ad-4a34-9d91-dd0e16619b35"
    , name = "My exported document 2"
    , state = DoneDocumentState
    , durability = PersistentDocumentDurability
    , projectUuid = Just project2.uuid
    , projectEventUuid = Just . getUuid . last $ project2Events
    , projectRepliesHash = hash . M.toList $ project2Ctn.replies
    , documentTemplateId = wizardDocumentTemplate.tId
    , formatUuid = formatJson.uuid
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
    , projectUuid = Just project2.uuid
    , projectEventUuid = Just . getUuid . last $ project2Events
    , projectRepliesHash = hash . M.toList $ project2Ctn.replies
    , documentTemplateId = wizardDocumentTemplate.tId
    , formatUuid = formatJson.uuid
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
    , projectUuid = Just differentProject.uuid
    , projectEventUuid = Nothing
    , projectRepliesHash = hash . M.toList $ project1Ctn.replies
    , documentTemplateId = anotherWizardDocumentTemplate.tId
    , formatUuid = formatJson.uuid
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
