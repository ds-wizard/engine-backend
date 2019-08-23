module Integration.Http.Typehint.Runner
  ( retrieveTypehints
  ) where

import Data.Map.Strict as M

import Integration.Http.Common.HttpClient
import Integration.Http.Typehint.RequestMapper
import Integration.Http.Typehint.ResponseMapper
import Integration.Resource.Typehint.TypehintIDTO
import Model.Context.AppContext
import Model.Error.Error
import Model.KnowledgeModel.KnowledgeModel

retrieveTypehints :: Integration -> M.Map String String -> AppContextM (Either AppError [TypehintIDTO])
retrieveTypehints integrationConfig variables =
  runRequest (toRetrieveTypehintsRequest integrationConfig variables) (toRetrieveTypehintsResponse integrationConfig)
