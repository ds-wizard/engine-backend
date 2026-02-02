module Shared.DocumentTemplate.Service.DocumentTemplate.DocumentTemplateUtil where

import qualified Data.UUID as U

import Shared.Common.Model.Context.AppContext
import Shared.Common.Util.List (groupBy)
import Shared.Coordinate.Model.Coordinate.Coordinate
import Shared.DocumentTemplate.Database.DAO.DocumentTemplate.DocumentTemplateDAO
import Shared.DocumentTemplate.Model.DocumentTemplate.DocumentTemplate

groupDocumentTemplates :: [DocumentTemplate] -> [[DocumentTemplate]]
groupDocumentTemplates =
  groupBy (\t1 t2 -> t1.organizationId == t2.organizationId && t1.templateId == t2.templateId)

resolveDocumentTemplateCoordinate :: AppContextC s sc m => Coordinate -> m DocumentTemplate
resolveDocumentTemplateCoordinate coordinate =
  if coordinate.version == "latest"
    then findLatestDocumentTemplateByOrganizationIdAndTemplateId coordinate.organizationId coordinate.entityId
    else findDocumentTemplateByCoordinate coordinate

changeDocumentTemplateIdInFormats :: U.UUID -> U.UUID -> [DocumentTemplateFormat] -> [DocumentTemplateFormat]
changeDocumentTemplateIdInFormats documentTemplateUuid tenantUuid =
  fmap
    ( \f ->
        f
          { documentTemplateUuid = documentTemplateUuid
          , steps =
              fmap
                ( \s ->
                    s
                      { documentTemplateUuid = documentTemplateUuid
                      , tenantUuid = tenantUuid
                      }
                    :: DocumentTemplateFormatStep
                )
                (steps f)
          , tenantUuid = tenantUuid
          }
        :: DocumentTemplateFormat
    )
