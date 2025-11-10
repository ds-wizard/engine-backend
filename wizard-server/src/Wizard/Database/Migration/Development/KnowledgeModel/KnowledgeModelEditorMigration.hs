module Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelEditorMigration where

import Data.Foldable (traverse_)

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorDAO
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelEditorEventDAO (insertKnowledgeModelEvent)
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Editor.KnowledgeModelEditors
import Wizard.Database.Migration.Development.User.Data.Users
import Wizard.Model.KnowledgeModel.Editor.KnowledgeModelEditorList
import Wizard.Service.KnowledgeModel.Editor.EditorService
import Wizard.Service.User.UserMapper

runMigration = do
  logInfo _CMP_MIGRATION "(KnowledgeModel/KnowledgeModelEditor) started"
  deleteKnowledgeModelEditors
  createEditorWithParams
    amsterdamKnowledgeModelEditorList.uuid
    amsterdamKnowledgeModelEditorList.createdAt
    (toDTO userAlbert)
    amsterdamKnowledgeModelEditorCreate
  traverse_ insertKnowledgeModelEvent amsterdamKnowledgeModelEditorEvents
  insertKnowledgeModelEditor differentKnowledgeModelEditor
  logInfo _CMP_MIGRATION "(KnowledgeModel/KnowledgeModelEditor) ended"
