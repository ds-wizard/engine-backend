module Service.KnowledgeModel.KnowledgeModelValidation
  ( validateKmValidity
  -- Helpers
  , heValidateKmValidity
  ) where

import Model.Context.AppContext
import Model.Error.Error
import Model.Event.Event
import Service.KnowledgeModel.KnowledgeModelApplicator
import Service.Package.PackageService

validateKmValidity :: [Event] -> Maybe String -> AppContextM (Maybe AppError)
validateKmValidity pkgEvents mParentPkgId =
  case mParentPkgId of
    Just parentPkgId -> do
      hmGetAllPreviousEventsSincePackageId parentPkgId $ \eventsFromParentPkg ->
        hmCompileKnowledgeModelFromScratch (eventsFromParentPkg ++ pkgEvents) $ \_ -> return Nothing
    Nothing -> hmCompileKnowledgeModelFromScratch pkgEvents $ \_ -> return Nothing

-- --------------------------------
-- HELPERS
-- --------------------------------
heValidateKmValidity pkgEvents mParentPkgId callback = do
  maybeError <- validateKmValidity pkgEvents mParentPkgId
  case maybeError of
    Nothing -> callback
    Just error -> return . Left $ error
