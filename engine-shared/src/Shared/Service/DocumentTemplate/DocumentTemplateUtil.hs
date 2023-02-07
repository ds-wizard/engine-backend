module Shared.Service.DocumentTemplate.DocumentTemplateUtil where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import qualified Data.List as L
import Data.Pool
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Records
import Shared.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.Model.DocumentTemplate.DocumentTemplate
import Shared.Model.Error.Error
import Shared.Service.Coordinate.CoordinateValidation
import Shared.Util.Coordinate
import Shared.Util.List (groupBy)

groupDocumentTemplates :: [DocumentTemplate] -> [[DocumentTemplate]]
groupDocumentTemplates =
  groupBy (\t1 t2 -> t1.organizationId == t2.organizationId && t1.templateId == t2.templateId)

resolveDocumentTemplateId
  :: ( MonadError AppError m
     , MonadLogger m
     , MonadReader s m
     , HasField "dbPool'" s (Pool Connection)
     , HasField "dbConnection'" s (Maybe Connection)
     , HasField "identityUuid'" s (Maybe String)
     , HasField "traceUuid'" s U.UUID
     , HasField "appUuid'" s U.UUID
     , MonadIO m
     )
  => String
  -> m String
resolveDocumentTemplateId coordinate = do
  validateCoordinateFormat True coordinate
  let version = getVersionFromCoordinate coordinate
  if version == "latest"
    then do
      let orgId = getOrgIdFromCoordinate coordinate
      let templateId = getTemplateIdFromCoordinate coordinate
      latest <- getLatestDocumentTemplateVersion orgId templateId
      return $ buildCoordinate orgId templateId latest
    else return coordinate

getLatestDocumentTemplateVersion orgId tId = do
  versions <- findVersionsForDocumentTemplate orgId tId
  return $ L.maximumBy compareVersion versions
