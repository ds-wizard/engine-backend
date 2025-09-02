module WizardLib.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateUtil where

import qualified Data.List as L
import qualified Data.UUID as U

import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.List (groupBy)
import WizardLib.Common.Service.Coordinate.CoordinateValidation
import WizardLib.Common.Util.Coordinate
import WizardLib.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import WizardLib.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

groupDocumentTemplates :: [DocumentTemplate] -> [[DocumentTemplate]]
groupDocumentTemplates =
  groupBy (\t1 t2 -> t1.organizationId == t2.organizationId && t1.templateId == t2.templateId)

resolveDocumentTemplateId :: AppContextC s sc m => String -> m String
resolveDocumentTemplateId coordinate = do
  validateCoordinateFormat True "templateId" coordinate
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

changeDocumentTemplateIdInFormats :: String -> U.UUID -> [DocumentTemplateFormat] -> [DocumentTemplateFormat]
changeDocumentTemplateIdInFormats documentTemplateId tenantUuid =
  fmap
    ( \f ->
        f
          { documentTemplateId = documentTemplateId
          , steps =
              fmap
                ( \s ->
                    s
                      { documentTemplateId = documentTemplateId
                      , tenantUuid = tenantUuid
                      }
                    :: DocumentTemplateFormatStep
                )
                (steps f)
          , tenantUuid = tenantUuid
          }
        :: DocumentTemplateFormat
    )
