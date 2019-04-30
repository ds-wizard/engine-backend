module DSW.Metamodel.Migrator.EventMigrator
  ( migrate
  ) where

import Data.Aeson

import qualified DSW.Metamodel.Migration.Migration1 as M1

type Version = Int

migrate :: Version -> Version -> Value -> Either String Value
migrate vSrc vDst input
  | vSrc > vDst = Left "Downgrade not supported"
  | vSrc == vDst = Right input
  | vSrc == 1 && vDst == 2 = M1.migrateEventValue input
  | otherwise = Left "Unsupported metamodel version"
