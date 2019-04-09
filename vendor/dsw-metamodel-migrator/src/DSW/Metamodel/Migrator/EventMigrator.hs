module DSW.Metamodel.Migrator.EventMigrator
  ( migrate
  ) where

import Data.Aeson

type Version = Int

migrate :: Version -> Version -> Value -> Either String Value
migrate vSrc vDst input
  | vSrc > vDst = Left "Downgrade not supported"
  | vSrc == vDst = Right input
  | otherwise = Left "Unsupported metamodel version"
