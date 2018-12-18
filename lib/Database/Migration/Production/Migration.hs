module Database.Migration.Production.Migration
  ( runMigration
  ) where

import Control.Lens ((^.))
import qualified
       Database.Migration.Production.Migration_0001_organization_init.Migration
       as M_0001
import qualified
       Database.Migration.Production.Migration_0002_users_init.Migration
       as M_0002
import qualified
       Database.Migration.Production.Migration_0003_book_references_init.Migration
       as M_0003
import qualified
       Database.Migration.Production.Migration_0004_metrics_init.Migration
       as M_0004
import qualified
       Database.Migration.Production.Migration_0005_levels_init.Migration
       as M_0005
import qualified
       Database.Migration.Production.Migration_0006_questionnaire_visibility.Migration
       as M_0006
import qualified
       Database.Migration.Production.Migration_0007_user_isActive.Migration
       as M_0007
import qualified
       Database.Migration.Production.Migration_0008_public_questionnaire_visibility.Migration
       as M_0008
import qualified
       Database.Migration.Production.Migration_0009_book_references_markdown.Migration
       as M_0009
import qualified
       Database.Migration.Production.Migration_0010_branch_owner_and_timestamp.Migration
       as M_0010
import qualified
       Database.Migration.Production.Migration_0011_update_data_steward_perms.Migration
       as M_0011
import qualified
       Database.Migration.Production.Migration_0012_erase_questionnaire_replies.Migration
       as M_0012
import Database.MongoDB.Migration.Entity
import Database.MongoDB.Migration.Migration
import LensesConfig

runMigration baseContext = do
  migrateDatabase (baseContext ^. pool) migrationDefinitions
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
  ]
