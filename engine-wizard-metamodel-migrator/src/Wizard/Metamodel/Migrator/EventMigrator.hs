module Wizard.Metamodel.Migrator.EventMigrator
  ( migrate
  ) where

import Data.Aeson
import Data.Maybe (fromJust, isJust)

import qualified Wizard.Metamodel.Migration.Migration1 as M1
import qualified Wizard.Metamodel.Migration.Migration2 as M2
import qualified Wizard.Metamodel.Migration.Migration3 as M3

type Version = Int

type ValueMigration = Value -> Either String Value

migrations :: [(Int, ValueMigration)]
migrations = [(1, M1.migrateEventValue), (2, M2.migrateEventValue), (3, M3.migrateEventValue)]

migrate :: Version -> Version -> Value -> Either String Value
migrate vSrc vDst input
  | vSrc > vDst = Left "Downgrade not supported"
  | vSrc == vDst = Right input
  | isJust migration = do
    migrated <- (fromJust migration) input
    migrate (vSrc + 1) vDst migrated
  | otherwise = Left "Unsupported metamodel version"
  where
    migration = lookup vSrc migrations
