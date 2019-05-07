module Service.KnowledgeModel.KnowledgeModelValidation
  ( validateKmValidity
  -- Helpers
  , heValidateKmValidity
  ) where

import Model.Context.AppContext
import Model.Error.Error
import Model.Event.Event
import Service.KnowledgeModel.KnowledgeModelService

validateKmValidity :: [Event] -> Maybe String -> AppContextM (Maybe AppError)
validateKmValidity events mPackageId = hmCompileKnowledgeModel events mPackageId [] $ \_ -> return Nothing

-- --------------------------------
-- HELPERS
-- --------------------------------
heValidateKmValidity pkgEvents mParentPkgId callback = do
  maybeError <- validateKmValidity pkgEvents mParentPkgId
  case maybeError of
    Nothing -> callback
    Just error -> return . Left $ error
