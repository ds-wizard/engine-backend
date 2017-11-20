module Service.Migrator.Methods.DiffTreeMethod where

import Control.Lens ((^.))

import Model.Event.Event
import Model.Migrator.MigrationState
import Service.Migrator.Methods.Common

isDiffTree :: MigrationState -> Event -> Bool
isDiffTree state event = firstCondition
  where
    firstCondition =
      if isDeleteAction event
        then foldl foldFun False eventWithSameTargetUuid
        else False
      where
        foldFun acc e = isEditAction e || acc
    eventWithSameTargetUuid = getEventsByTargetUuid (getTargetUuid event) (state ^. msParentEvents)
