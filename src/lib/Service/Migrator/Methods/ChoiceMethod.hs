module Service.Migrator.Methods.ChoiceMethod where

import Control.Lens ((^.))

import Model.Event.Event
import Model.Migrator.MigrationState
import Service.Migrator.Methods.Common

isChoice :: MigrationState -> Event -> Bool
isChoice state event = firstCondition || secondCondition
  where
    firstCondition =
      if isEditAction event
        then foldl foldFun False eventWithSameTargetUuid
        else False
      where
        foldFun acc e = isEditAction e || isDeleteAction e || acc
    secondCondition =
      if isDeleteAction event
        then foldl foldFun False eventWithSameTargetUuid
        else False
      where
        foldFun acc e = isEditAction e || acc
    eventWithSameTargetUuid =
      getEventsByTargetUuid (getTargetUuid event) (state ^. msParentEvents)
