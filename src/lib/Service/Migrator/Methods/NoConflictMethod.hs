module Service.Migrator.Methods.NoConflictMethod where

import Control.Lens ((^.), (&), (.~))
import Data.Either

import Model.Event.Event
import Model.Migrator.MigrationState
import Service.Migrator.Applicator

runNoConflictMethod :: MigrationState -> Event -> MigrationState
runNoConflictMethod state event =
  let eitherNewKm = runApplicator (state ^. msCurrentKnowledgeModel) [event]
  in if isRight eitherNewKm
  then
    let (Right newKm) = eitherNewKm
    in state & msCurrentKnowledgeModel .~ Just newKm
  else
    let (Left error) = eitherNewKm
    in convertToErrorState state (ApplicatorError $ "Error in compilation of Knowledge Model in 'NoConflictMethod' (" ++ (show error) ++ ")")