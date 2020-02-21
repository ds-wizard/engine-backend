module Wizard.Integration.Http.Typehint.Runner
  ( retrieveTypehints
  ) where

import Data.Map.Strict as M

import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Integration.Http.Common.HttpClient
import Wizard.Integration.Http.Typehint.RequestMapper
import Wizard.Integration.Http.Typehint.ResponseMapper
import Wizard.Integration.Resource.Typehint.TypehintIDTO
import Wizard.Model.Context.AppContext

retrieveTypehints :: Integration -> M.Map String String -> AppContextM [TypehintIDTO]
retrieveTypehints integrationConfig variables =
  runRequest (toRetrieveTypehintsRequest integrationConfig variables) (toRetrieveTypehintsResponse integrationConfig)
