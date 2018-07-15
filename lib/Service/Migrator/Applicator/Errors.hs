module Service.Migrator.Applicator.Errors where

import Common.Localization
import Model.Error.Error
import Model.Event.EventAccessors

errorPathShouldBeEmpty e path = Left . MigratorError $ _ERROR_MT_APPLICATOR__PATH_SHOULD_BE_EMPTY (getEventUuid e) path

errorEditNonExistingThing e = Left . MigratorError $ _ERROR_MT_APPLICATOR__EDIT_NON_EXISTING_THING (getEventUuid e)

errorEmptyPath e = Left . MigratorError $ _ERROR_MT_APPLICATOR__EMPTY_PATH (getEventUuid e)

errorIllegalState e eventName entityName =
  Left . MigratorError $ _ERROR_MT_APPLICATOR__ILLEGAL_STATE (getEventUuid e) eventName entityName
