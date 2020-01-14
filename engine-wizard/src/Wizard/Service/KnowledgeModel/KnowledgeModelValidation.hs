module Wizard.Service.KnowledgeModel.KnowledgeModelValidation
  ( validateKmValidity
  -- Helpers
  , heValidateKmValidity
  ) where

import Shared.Model.Error.Error
import Shared.Model.Event.Event
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.KnowledgeModelService

validateKmValidity :: [Event] -> Maybe String -> AppContextM (Maybe AppError)
validateKmValidity events mPackageId = hmCompileKnowledgeModel events mPackageId [] $ \_ -> return Nothing

-- --------------------------------
-- HELPERS
-- --------------------------------
heValidateKmValidity pkgEvents mPreviousPkgId callback = do
  maybeError <- validateKmValidity pkgEvents mPreviousPkgId
  case maybeError of
    Nothing -> callback
    Just error -> return . Left $ error
