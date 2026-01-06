module Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireSM where

import Data.Map.Strict as M
import Data.Swagger

import Shared.Common.Util.Swagger
import Shared.Common.Util.Uuid
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.KnowledgeModelSM ()
import Shared.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.KnowledgeModels
import Wizard.Api.Resource.Project.Acl.ProjectPermSM ()
import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireDTO
import Wizard.Api.Resource.Project.Detail.ProjectDetailQuestionnaireJM ()
import Wizard.Api.Resource.Project.File.ProjectFileSimpleSM ()
import Wizard.Api.Resource.Project.ProjectReplySM ()
import Wizard.Api.Resource.Project.ProjectSharingSM ()
import Wizard.Api.Resource.Project.ProjectVisibilitySM ()
import Wizard.Database.Migration.Development.Project.Data.ProjectLabels
import Wizard.Database.Migration.Development.Project.Data.ProjectReplies
import Wizard.Database.Migration.Development.Project.Data.Projects
import Wizard.Model.Project.Project

instance ToSchema ProjectDetailQuestionnaireDTO where
  declareNamedSchema =
    toSwagger $
      ProjectDetailQuestionnaireDTO
        { uuid = project1.uuid
        , name = project1.name
        , visibility = project1.visibility
        , sharing = project1.sharing
        , knowledgeModelPackageId = project1.knowledgeModelPackageId
        , selectedQuestionTagUuids = project1.selectedQuestionTagUuids
        , isTemplate = project1.isTemplate
        , knowledgeModel = km1
        , replies = fReplies
        , labels = fLabels
        , phaseUuid = Just . u' $ "4b376e49-1589-429b-9590-c654378f0bd5"
        , migrationUuid = Nothing
        , permissions = [project1AlbertEditProjectPermDto]
        , files = []
        , unresolvedCommentCounts =
            M.fromList
              [
                ( "4f61fdfa-ce82-41b5-a1e6-218beaf41660.0dc58313-eb80-4f74-a8c1-347b644665d5"
                , M.fromList [(u' "f1de85a9-7f22-4d0c-bc23-3315cc4c85d7", 4)]
                )
              ]
        , resolvedCommentCounts =
            M.fromList
              [
                ( "4f61fdfa-ce82-41b5-a1e6-218beaf41660.0dc58313-eb80-4f74-a8c1-347b644665d5"
                , M.fromList [(u' "f1de85a9-7f22-4d0c-bc23-3315cc4c85d7", 2)]
                )
              ]
        , projectActionsAvailable = 1
        , projectImportersAvailable = 2
        , fileCount = 0
        }
