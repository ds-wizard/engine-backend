module Service.Migrator.Methods.Common where

import qualified Data.UUID as U

import Model.Event.Event

isThereEventWithGivenTargetUuid :: U.UUID -> [Event] -> Bool
isThereEventWithGivenTargetUuid uuid events = length (getEventsByTargetUuid uuid events) > 0

getEventsByTargetUuid :: U.UUID -> [Event] -> [Event]
getEventsByTargetUuid uuid = foldl foldEvent []
  where
    foldEvent foundEvents event =
      if getTargetUuid event == uuid
        then foundEvents ++ [event]
        else foundEvents
