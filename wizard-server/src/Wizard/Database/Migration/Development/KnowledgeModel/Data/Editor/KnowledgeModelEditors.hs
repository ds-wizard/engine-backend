module Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors where

import Data.Either (rights)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Time

import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Constant.KnowledgeModel
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Event.KnowledgeModelEvents
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import qualified Shared.KnowledgeModel.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper as SPM
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorChangeDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorCreateDTO
import Wizard.Api.Resource.KnowledgeModel.Editor.KnowledgeModelEditorDetailDTO
import Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishEditorDTO
import Wizard.Api.Resource.KnowledgeModel.Package.Publish.KnowledgeModelPackagePublishMigrationDTO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Package.KnowledgeModelPackages
import Wizard.Database.Migration.Development.Tenant.Data.Tenants
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditor
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorEvent
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorState
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorSuggestion
import Wizard.Model.Questionnaire.QuestionnaireReply
import Wizard.Model.Tenant.Tenant
import Wizard.Model.User.User
import Wizard.Service.KnowledgeModel.Compiler.Compiler
import Wizard.Service.KnowledgeModel.Editor.EditorMapper
import qualified Wizard.Service.KnowledgeModel.Package.KnowledgeModelPackageMapper as PM

amsterdamKnowledgeModelEditorList :: KnowledgeModelEditorList
amsterdamKnowledgeModelEditorList =
  KnowledgeModelEditorList
    { uuid = u' "6474b24b-262b-42b1-9451-008e8363f2b6"
    , name = amsterdamKmPackage.name
    , kmId = amsterdamKmPackage.kmId
    , version = amsterdamKmPackage.version
    , previousPackageId = Just netherlandsKmPackage.pId
    , forkOfPackageId = Just netherlandsKmPackage.pId
    , state = EditedKnowledgeModelEditorState
    , createdBy = Just $ userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

amsterdamKnowledgeModelEditor :: KnowledgeModelEditor
amsterdamKnowledgeModelEditor =
  KnowledgeModelEditor
    { uuid = amsterdamKnowledgeModelEditorList.uuid
    , name = amsterdamKnowledgeModelEditorList.name
    , kmId = amsterdamKnowledgeModelEditorList.kmId
    , version = "1.0.0"
    , description = "First Release"
    , readme = "# Netherlands Knowledge Model"
    , license = "Apache-2.0"
    , previousPackageId = amsterdamKnowledgeModelEditorList.previousPackageId
    , metamodelVersion = knowledgeModelMetamodelVersion
    , squashed = True
    , createdBy = amsterdamKnowledgeModelEditorList.createdBy
    , tenantUuid = defaultTenant.uuid
    , createdAt = amsterdamKnowledgeModelEditorList.createdAt
    , updatedAt = amsterdamKnowledgeModelEditorList.updatedAt
    }

amsterdamKnowledgeModelEditorSuggestion :: KnowledgeModelEditorSuggestion
amsterdamKnowledgeModelEditorSuggestion =
  KnowledgeModelEditorSuggestion
    { uuid = amsterdamKnowledgeModelEditorList.uuid
    , name = amsterdamKnowledgeModelEditorList.name
    }

amsterdamKnowledgeModelEditorEvents :: [KnowledgeModelEditorEvent]
amsterdamKnowledgeModelEditorEvents = fmap (toKnowledgeModelEditorEvent amsterdamKnowledgeModelEditorList.uuid defaultTenant.uuid) amsterdamEvents

amsterdamEvents :: [KnowledgeModelEvent]
amsterdamEvents =
  [ a_km1_ir
  , a_km1_ch1_q1
  , a_km1_ch1_q2
  , a_km1_ch1_q2_aNo1
  , a_km1_ch1_q2_aYes1
  , a_km1_ch1_ansYes1_fuq1
  , a_km1_ch1_q2_aYes1_fuq1_aNo
  , a_km1_ch1_q2_aYesFu1
  , a_km1_ch1_q2_ansYes_fuq1_ansYes_fuq2
  , a_km1_ch1_q2_aNoFu2
  , a_km1_ch1_q2_aYesFu2
  , a_km1_ch1_q2_eAlbert
  , a_km1_ch1_q2_eNikola
  , a_km1_ch1_q2_rCh1
  , a_km1_ch1_q2_rCh2
  , a_km1_ch2
  , a_km1_ch2_q3
  , a_km1_ch2_q3_aNo2
  , a_km1_ch2_q3_aYes2
  , a_km1_ch3
  , a_km1_ch3_q15
  ]

amsterdamKnowledgeModelEditorReplies :: M.Map String Reply
amsterdamKnowledgeModelEditorReplies = M.empty

amsterdamKnowledgeModelEditorCreate :: KnowledgeModelEditorCreateDTO
amsterdamKnowledgeModelEditorCreate =
  KnowledgeModelEditorCreateDTO
    { name = amsterdamKnowledgeModelEditorList.name
    , kmId = amsterdamKnowledgeModelEditorList.kmId
    , version = "1.0.0"
    , previousPackageId = amsterdamKnowledgeModelEditorList.previousPackageId
    }

amsterdamKnowledgeModelEditorChange :: KnowledgeModelEditorChangeDTO
amsterdamKnowledgeModelEditorChange =
  KnowledgeModelEditorChangeDTO
    { name = "EDITED: " ++ amsterdamKnowledgeModelEditorList.name
    , kmId = amsterdamKnowledgeModelEditorList.kmId
    , version = "2.0.0"
    , description = "EDITED: description"
    , readme = "EDITED: Readme"
    , license = "Apache-3.0"
    }

amsterdamKnowledgeModelEditorKnowledgeModel :: KnowledgeModel
amsterdamKnowledgeModelEditorKnowledgeModel =
  head . rights $ [compile Nothing . fmap SPM.toEvent $ globalKmPackageEvents ++ netherlandsKmPackageEvents]

amsterdamKnowledgeModelEditorDetail :: KnowledgeModelEditorDetailDTO
amsterdamKnowledgeModelEditorDetail =
  KnowledgeModelEditorDetailDTO
    { uuid = amsterdamKnowledgeModelEditorList.uuid
    , name = amsterdamKnowledgeModelEditorList.name
    , kmId = amsterdamKnowledgeModelEditorList.kmId
    , version = amsterdamKnowledgeModelEditor.version
    , description = amsterdamKnowledgeModelEditor.description
    , readme = amsterdamKnowledgeModelEditor.readme
    , license = amsterdamKnowledgeModelEditor.license
    , state = EditedKnowledgeModelEditorState
    , previousPackageId = amsterdamKnowledgeModelEditorList.previousPackageId
    , forkOfPackageId = amsterdamKnowledgeModelEditorList.forkOfPackageId
    , forkOfPackage = Just . PM.toSimpleDTO $ netherlandsKmPackage
    , createdBy = amsterdamKnowledgeModelEditorList.createdBy
    , events = amsterdamEvents
    , replies = amsterdamKnowledgeModelEditorReplies
    , knowledgeModel = amsterdamKnowledgeModelEditorKnowledgeModel
    , createdAt = amsterdamKnowledgeModelEditorList.createdAt
    , updatedAt = amsterdamKnowledgeModelEditorList.updatedAt
    }

leidenKnowledgeModelEditor :: KnowledgeModelEditorList
leidenKnowledgeModelEditor =
  KnowledgeModelEditorList
    { uuid = u' "47421955-ba30-48d4-8c49-9ec47eda2cad"
    , name = "Leiden KM"
    , kmId = "leiden-km"
    , version = "1.0.0"
    , state = DefaultKnowledgeModelEditorState
    , previousPackageId = Just netherlandsKmPackage.pId
    , forkOfPackageId = Just netherlandsKmPackage.pId
    , createdBy = Just $ userAlbert.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

leidenKnowledgeModelEditorCreate :: KnowledgeModelEditorCreateDTO
leidenKnowledgeModelEditorCreate =
  KnowledgeModelEditorCreateDTO
    { name = leidenKnowledgeModelEditor.name
    , kmId = leidenKnowledgeModelEditor.kmId
    , version = "1.0.0"
    , previousPackageId = leidenKnowledgeModelEditor.previousPackageId
    }

differentKnowledgeModelEditor :: KnowledgeModelEditor
differentKnowledgeModelEditor =
  KnowledgeModelEditor
    { uuid = u' "fc49b6a5-51ae-4442-82e8-c3bf216545ec"
    , name = "KnowledgeModelEditor Events"
    , kmId = "my-km"
    , version = "1.0.0"
    , description = "Some desc"
    , readme = "Some readme"
    , license = "Apache-2.0"
    , previousPackageId = Just $ differentPackage.pId
    , metamodelVersion = knowledgeModelMetamodelVersion
    , squashed = True
    , createdBy = Just $ userCharles.uuid
    , tenantUuid = differentTenant.uuid
    , createdAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    , updatedAt = UTCTime (fromJust $ fromGregorianValid 2018 1 25) 0
    }

differentKnowledgeModelEditorReplies :: M.Map String Reply
differentKnowledgeModelEditorReplies = M.empty

packagePublishEditorDTO :: PackagePublishEditorDTO
packagePublishEditorDTO =
  PackagePublishEditorDTO
    { editorUuid = amsterdamKnowledgeModelEditor.uuid
    }

packagePublishMigrationDTO :: PackagePublishMigrationDTO
packagePublishMigrationDTO =
  PackagePublishMigrationDTO
    { editorUuid = amsterdamKnowledgeModelEditor.uuid
    , version = amsterdamKmPackage.version
    , description = amsterdamKmPackage.description
    , readme = amsterdamKmPackage.readme
    }
