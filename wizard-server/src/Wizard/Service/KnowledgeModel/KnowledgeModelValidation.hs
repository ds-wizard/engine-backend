module Wizard.Service.KnowledgeModel.KnowledgeModelValidation where

import qualified Data.UUID as U

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Wizard.Model.Context.AppContext
import Wizard.Service.KnowledgeModel.KnowledgeModelService

validateKmValidity :: [KnowledgeModelEvent] -> Maybe U.UUID -> AppContextM ()
validateKmValidity events mPkgUuid = do
  _ <- compileKnowledgeModel events mPkgUuid []
  return ()
