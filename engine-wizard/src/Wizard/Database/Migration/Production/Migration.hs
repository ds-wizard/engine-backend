module Wizard.Database.Migration.Production.Migration
  ( runMigration
  ) where

import Control.Lens ((^.))
import Database.MongoDB.Migration.Entity
import Database.MongoDB.Migration.Migration
import LensesConfig
import qualified Wizard.Database.Migration.Production.Migration_0001_organization_init.Migration as M_0001
import qualified Wizard.Database.Migration.Production.Migration_0002_users_init.Migration as M_0002
import qualified Wizard.Database.Migration.Production.Migration_0003_book_references_init.Migration as M_0003
import qualified Wizard.Database.Migration.Production.Migration_0004_metrics_init.Migration as M_0004
import qualified Wizard.Database.Migration.Production.Migration_0005_levels_init.Migration as M_0005
import qualified Wizard.Database.Migration.Production.Migration_0006_questionnaire_visibility.Migration as M_0006
import qualified Wizard.Database.Migration.Production.Migration_0007_user_isActive.Migration as M_0007
import qualified Wizard.Database.Migration.Production.Migration_0008_public_questionnaire_visibility.Migration as M_0008
import qualified Wizard.Database.Migration.Production.Migration_0009_book_references_markdown.Migration as M_0009
import qualified Wizard.Database.Migration.Production.Migration_0010_branch_owner_and_timestamp.Migration as M_0010
import qualified Wizard.Database.Migration.Production.Migration_0011_update_data_steward_perms.Migration as M_0011
import qualified Wizard.Database.Migration.Production.Migration_0012_erase_questionnaire_replies.Migration as M_0012
import qualified Wizard.Database.Migration.Production.Migration_0013_questionnaire_tagUuids.Migration as M_0013
import qualified Wizard.Database.Migration.Production.Migration_0014_purge_database.Migration as M_0014
import qualified Wizard.Database.Migration.Production.Migration_0015_remove_cached_km.Migration as M_0015
import qualified Wizard.Database.Migration.Production.Migration_0016_metamodel_version.Migration as M_0016
import qualified Wizard.Database.Migration.Production.Migration_0017_questionnaire_accessibility.Migration as M_0017
import qualified Wizard.Database.Migration.Production.Migration_0018_package_readme_and_createdAt.Migration as M_0018
import qualified Wizard.Database.Migration.Production.Migration_0019_bson_hashmap.Migration as M_0019
import qualified Wizard.Database.Migration.Production.Migration_0020_package_license.Migration as M_0020
import qualified Wizard.Database.Migration.Production.Migration_0021_questionniare_labels.Migration as M_0021
import qualified Wizard.Database.Migration.Production.Migration_0022_forkOfPackageId_and_mergeCheckpointPackageId.Migration as M_0022
import qualified Wizard.Database.Migration.Production.Migration_0023_remove_itemTitle.Migration as M_0023
import qualified Wizard.Database.Migration.Production.Migration_0024_user_name_and_surname.Migration as M_0024
import qualified Wizard.Database.Migration.Production.Migration_0025_document_preview_and_formatUuid.Migration as M_0025
import qualified Wizard.Database.Migration.Production.Migration_0026_add_application_configs.Migration as M_0026
import qualified Wizard.Database.Migration.Production.Migration_0027_submission.Migration as M_0027
import qualified Wizard.Database.Migration.Production.Migration_0028_questionnaire_creator.Migration as M_0028
import qualified Wizard.Database.Migration.Production.Migration_0029_add_db_indexes.Migration as M_0029
import qualified Wizard.Database.Migration.Production.Migration_0030_bson_generic.Migration as M_0030
import qualified Wizard.Database.Migration.Production.Migration_0031_user_imageUrl.Migration as M_0031
import qualified Wizard.Database.Migration.Production.Migration_0032_appConfig_organizationDescription.Migration as M_0032
import qualified Wizard.Database.Migration.Production.Migration_0033_remove_public_questionnaire.Migration as M_0033
import qualified Wizard.Database.Migration.Production.Migration_0034_appConfig_recommendedTemplateUuid.Migration as M_0034
import Wizard.Util.Logger

runMigration baseContext = do
  migrateDatabase (baseContext ^. pool) migrationDefinitions (logInfo _CMP_MIGRATION)
  return ()

migrationDefinitions :: [MigrationDefinition]
migrationDefinitions =
  [ M_0001.definition
  , M_0002.definition
  , M_0003.definition
  , M_0004.definition
  , M_0005.definition
  , M_0006.definition
  , M_0007.definition
  , M_0008.definition
  , M_0009.definition
  , M_0010.definition
  , M_0011.definition
  , M_0012.definition
  , M_0013.definition
  , M_0014.definition
  , M_0015.definition
  , M_0016.definition
  , M_0017.definition
  , M_0018.definition
  , M_0019.definition
  , M_0020.definition
  , M_0021.definition
  , M_0022.definition
  , M_0023.definition
  , M_0024.definition
  , M_0025.definition
  , M_0026.definition
  , M_0027.definition
  , M_0028.definition
  , M_0029.definition
  , M_0030.definition
  , M_0031.definition
  , M_0032.definition
  , M_0033.definition
  , M_0034.definition
  ]
