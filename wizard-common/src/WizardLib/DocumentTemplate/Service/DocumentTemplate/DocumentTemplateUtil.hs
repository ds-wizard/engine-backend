module WizardLib.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateUtil where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader (MonadReader)
import qualified Data.List as L
import Data.Pool
import qualified Data.UUID as U
import Database.PostgreSQL.Simple
import GHC.Records
import Shared.Common.Model.Error.Error
import Shared.Common.Util.List (groupBy)
import WizardLib.Common.Service.Coordinate.CoordinateValidation
import WizardLib.Common.Util.Coordinate
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

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
