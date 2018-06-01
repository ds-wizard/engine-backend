module Service.Migrator.Applicator.Errors where

import Control.Lens ((^.))

import Common.Error
import Common.Localization
import LensesConfig

errorPathShouldBeEmpty e path = Left . MigratorError $ _ERROR_MT_APPLICATOR__PATH_SHOULD_BE_EMPTY (e ^. uuid) path

errorEditNonExistingThing e = Left . MigratorError $ _ERROR_MT_APPLICATOR__EDIT_NON_EXISTING_THING (e ^. uuid)

errorEmptyPath e = Left . MigratorError $ _ERROR_MT_APPLICATOR__EMPTY_PATH (e ^. uuid)

errorIllegalState e eventName entityName =
  Left . MigratorError $ _ERROR_MT_APPLICATOR__ILLEGAL_STATE (e ^. uuid) eventName entityName
