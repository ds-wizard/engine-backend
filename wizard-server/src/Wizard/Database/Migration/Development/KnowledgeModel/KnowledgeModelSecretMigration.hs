module Wizard.Database.Migration.Development.KnowledgeModel.KnowledgeModelSecretMigration where

import Shared.Common.Constant.Component
import Shared.Common.Util.Logger
import Wizard.Database.DAO.KnowledgeModel.KnowledgeModelSecretDAO
import Wizard.Database.Migration.Development.KnowledgeModel.Data.Secret.KnowledgeModelSecrets

runMigration = do
  logInfo _CMP_MIGRATION "(KnowledgeModelSecret/KnowledgeModelSecret) started"
  deleteKnowledgeModelSecrets
  insertKnowledgeModelSecret kmSecret1
  insertKnowledgeModelSecret kmSecretDifferent
  logInfo _CMP_MIGRATION "(KnowledgeModelSecret/KnowledgeModelSecret) ended"
