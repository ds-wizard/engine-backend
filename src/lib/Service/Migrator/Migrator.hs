module Service.Migrator.Migrator where

import Control.Lens ((^.), (&), (.~), makeLenses)
import Data.Either
import qualified Data.UUID as U

import Common.Context
import Common.Error
import Database.DAO.Package.PackageDAO
import Model.Event.Event
import Model.KnowledgeModel.KnowledgeModel
import Model.Migrator.MigrationState
import Model.Package.Package
import Service.Migrator.Applicator
import Service.Migrator.Methods.ChoiceMethod
import Service.Migrator.Methods.Common
import Service.Migrator.Methods.DiffTreeMethod
import Service.Migrator.Methods.NoConflictMethod

createMigration :: Context -> String -> String -> IO (Either AppError MigrationState)
createMigration context parentPackageId localizationPackageId = do
  eitherParentPackage <- findPackageWithEventsById context parentPackageId
  case eitherParentPackage of
    Left error -> return . Left $ error
    Right parentPackage -> do
      eitherLocalizationPackage <- findPackageWithEventsById context localizationPackageId
      case eitherLocalizationPackage of
        Left error -> return . Left $ error
        Right localizationPackage ->
          return . Right $
          createMigrationStateWithEvents
            parentPackageId
            localizationPackageId
            (parentPackage ^. pkgweEvents)
            (localizationPackage ^. pkgweEvents)

createMigrationStateWithEvents parentPackageId localizationPackageId parentEvents localizationEvents =
  MigrationState
  { _msStatus = MSCreated
  , _msParentPackageId = parentPackageId
  , _msLocalizationPackageId = localizationPackageId
  , _msCurrentKnowledgeModel = Nothing
  , _msError = NoError
  , _msParentEvents = parentEvents
  , _msLocalizationEvents = localizationEvents
  , _msDiffTable = buildDiffTable parentEvents
  , _msDiffTree = Nothing
  }

doMigrate :: MigrationState -> Event -> MigrationState
doMigrate state event =
  let newState = runNoConflictMethod state event
      (_:newLocalizationEvents) = newState ^. msLocalizationEvents
  in newState & msLocalizationEvents .~ newLocalizationEvents

migrate :: MigrationState -> MigrationState
migrate state =
  case state ^. msStatus of
    MSCreated ->
      let eitherKm = runApplicator Nothing (state ^. msParentEvents)
      in case eitherKm of
           Left error -> state & msError .~ ApplicatorError "Error in compiling future parent Knowledge Model events"
           Right km ->
             let newState = state & msStatus .~ MSRunning
--                 newState2 = state & msDiffTree .~ Just (buildDiffTree km)
--             in migrate $ newState2 & msCurrentKnowledgeModel .~ Just km
             in migrate $ newState & msCurrentKnowledgeModel .~ Just km
    MSRunning ->
      let newState = foldl doMigrate state (state ^. msLocalizationEvents)
      in if newState ^. msLocalizationEvents == []
           then newState & msStatus .~ MSCompleted
           else newState
    MSError -> state
    MSCompleted -> state
--{
--	"state": "RUNNING|CONFLICT|COMPLETED",
--	"previousParentPackageId": "elixir.base:core:0.0.1",
--	"futureParentPackageId": "elixir.base:core:0.0.2",
--	"currentKnowledgeModel": {
--		"kmUuid": "b0edbc0b-2d7d-4ee7-bf2f-bc3a22d7494f",
--		"name": "My Knowledge Model",
--		"chapters": []
--	},
--	"conflict": {
--		"type": "NO_CONFLICT|CHOICE|DIFF_TREE|POOL"
--	},
--	"localizationEvents": []
--}
