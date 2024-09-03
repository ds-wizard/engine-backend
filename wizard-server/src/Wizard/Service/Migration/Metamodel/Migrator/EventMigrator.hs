module Wizard.Service.Migration.Metamodel.Migrator.EventMigrator (
  migrate,
) where

import Data.Aeson (Value)
import Data.Maybe (fromJust, isJust)

import Shared.Common.Util.List (foldEither)
import qualified Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0001 as M1
import qualified Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0002 as M2
import qualified Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0003 as M3
import qualified Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0004 as M4
import qualified Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0005 as M5
import qualified Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0006 as M6
import qualified Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0007 as M7
import qualified Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0008 as M8
import qualified Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0009 as M9
import qualified Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0010 as M10
import qualified Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0011 as M11
import qualified Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0012 as M12
import qualified Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0013 as M13
import qualified Wizard.Service.Migration.Metamodel.Migrator.Migrations.Migration0014 as M14
import Wizard.Service.Migration.Metamodel.Migrator.Migrations.MigrationContext (MigrationContext)

type Version = Int

type ValueMigration = MigrationContext -> Value -> Either String [Value]

migrations :: [(Int, ValueMigration)]
migrations =
  [ (1, M1.migrateEventValue)
  , (2, M2.migrateEventValue)
  , (3, M3.migrateEventValue)
  , (4, M4.migrateEventValue)
  , (5, M5.migrateEventValue)
  , (6, M6.migrateEventValue)
  , (7, M7.migrateEventValue)
  , (8, M8.migrateEventValue)
  , (9, M9.migrateEventValue)
  , (10, M10.migrateEventValue)
  , (11, M11.migrateEventValue)
  , (12, M12.migrateEventValue)
  , (13, M13.migrateEventValue)
  , (14, M14.migrateEventValue)
  ]

migrate :: MigrationContext -> Version -> Version -> Value -> Either String [Value]
migrate ctx vSrc vDst input
  | vSrc > vDst = Left "Downgrade not supported"
  | vSrc == vDst = Right [input]
  | isJust migration = do
      migrated <- fromJust migration ctx input
      fmap concat . foldEither . fmap (migrate ctx (vSrc + 1) vDst) $ migrated
  | otherwise = Left "Unsupported metamodel version"
  where
    migration = lookup vSrc migrations
