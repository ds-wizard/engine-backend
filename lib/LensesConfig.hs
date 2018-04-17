module LensesConfig where

import Control.Lens (makeFields)

import Api.Resource.ActionKey.ActionKeyDTO
import Api.Resource.KnowledgeModel.KnowledgeModelDTO
import Model.ActionKey.ActionKey
import Model.Config.DSWConfig
import Model.Context.AppContext
import Model.KnowledgeModel.KnowledgeModel

-- -------------------------------------
-- Model
-- -------------------------------------
-- Model / ActionKey
makeFields ''ActionKey

-- Model / Config
makeFields ''AppConfigClient

makeFields ''AppConfigWeb

makeFields ''AppConfigDatabase

makeFields ''AppConfigJwt

makeFields ''AppConfigRoles

makeFields ''AppConfigMail

makeFields ''BuildInfo

makeFields ''DSWConfig

-- Model / Config
makeFields ''AppContext

-- Model / KnowledgeModel
makeFields ''KnowledgeModel

makeFields ''Chapter

makeFields ''Question

makeFields ''Answer

makeFields ''Expert

makeFields ''Reference

-- -------------------------------------
-- Api / Resource
-- -------------------------------------
-- Api / Resource / ActionKeyDTO
makeFields ''ActionKeyDTO

-- Api / Resource / KnowledgeModelDTO
makeFields ''KnowledgeModelDTO

makeFields ''ChapterDTO

makeFields ''QuestionDTO

makeFields ''AnswerDTO

makeFields ''ExpertDTO

makeFields ''ReferenceDTO
