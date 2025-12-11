module Registry.Database.Migration.Production.Migration where

import Database.PostgreSQL.Migration.Entity

import qualified Registry.Database.Migration.Production.Migration_0001_init.Migration as M_0001
import qualified Registry.Database.Migration.Production.Migration_0002_app.Migration as M_0002
import qualified Registry.Database.Migration.Production.Migration_0003_persistentCommand.Migration as M_0003
import qualified Registry.Database.Migration.Production.Migration_0004_appLimit.Migration as M_0004
import qualified Registry.Database.Migration.Production.Migration_0005_locale.Migration as M_0005
import qualified Registry.Database.Migration.Production.Migration_0006_templateTimestamps.Migration as M_0006
import qualified Registry.Database.Migration.Production.Migration_0007_component.Migration as M_0007
import qualified Registry.Database.Migration.Production.Migration_0008_unification.Migration as M_0008
import qualified Registry.Database.Migration.Production.Migration_0009_persistentCommandDestination.Migration as M_0009
import qualified Registry.Database.Migration.Production.Migration_0010_pkgAndDocReadOnly.Migration as M_0010
import qualified Registry.Database.Migration.Production.Migration_0011_traceUuid.Migration as M_0011
import qualified Registry.Database.Migration.Production.Migration_0012_tenant.Migration as M_0012
import qualified Registry.Database.Migration.Production.Migration_0013_jsonb.Migration as M_0013
import qualified Registry.Database.Migration.Production.Migration_0014_documentTemplateMetamodel.Migration as M_0014
import qualified Registry.Database.Migration.Production.Migration_0015_knowledgeModelRefactor.Migration as M_0015
import qualified Registry.Database.Migration.Production.Migration_0016_project.Migration as M_0016

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
  ]
