module Shared.Service.Template.TemplateUtil where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import qualified Data.List as L
import Data.Pool
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Records
import Shared.Database.DAO.Template.TemplateDAO
import Shared.Model.Error.Error
import Shared.Model.Template.Template
import Shared.Service.Coordinate.CoordinateValidation
import Shared.Util.Coordinate
import Shared.Util.List (groupBy)

groupTemplates :: [Template] -> [[Template]]
groupTemplates =
  groupBy (\t1 t2 -> t1.organizationId == t2.organizationId && t1.templateId == t2.templateId)

resolveTemplateId
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
resolveTemplateId coordinate = do
  validateCoordinateFormat True coordinate
  let version = getVersionFromCoordinate coordinate
  if version == "latest"
    then do
      let orgId = getOrgIdFromCoordinate coordinate
      let templateId = getTemplateIdFromCoordinate coordinate
      latest <- getLatestTemplateVersion orgId templateId
      return $ buildCoordinate orgId templateId latest
    else return coordinate

getLatestTemplateVersion orgId tId = do
  versions <- findVersionsForTemplate orgId tId
  return $ L.maximumBy compareVersion versions
