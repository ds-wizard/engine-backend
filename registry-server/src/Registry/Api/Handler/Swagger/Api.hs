module Registry.Api.Handler.Swagger.Api where

import Data.Swagger
import Servant
import Servant.Swagger
import Servant.Swagger.UI

import Registry.Api.Handler.Api
import Registry.Api.Resource.ActionKey.ActionKeySM ()
import Registry.Api.Resource.Config.ClientConfigSM ()
import Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailSM ()
import Registry.Api.Resource.DocumentTemplate.DocumentTemplateSimpleSM ()
import Registry.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleSM ()
import Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageDetailSM ()
import Registry.Api.Resource.KnowledgeModel.Package.KnowledgeModelPackageSimpleSM ()
import Registry.Api.Resource.Locale.LocaleDetailSM ()
import Registry.Api.Resource.Locale.LocaleSM ()
import Registry.Api.Resource.Organization.OrganizationChangeSM ()
import Registry.Api.Resource.Organization.OrganizationCreateSM ()
import Registry.Api.Resource.Organization.OrganizationSM ()
import Registry.Api.Resource.Organization.OrganizationStateSM ()
import Registry.Api.Resource.PersistentCommand.PersistentCommandSM ()
import Shared.Common.Api.Resource.Common.FileSM ()
import Shared.Common.Api.Resource.Common.SemVer2TupleSM ()
import Shared.Common.Api.Resource.Info.InfoSM ()
import Shared.Component.Api.Resource.Component.ComponentSM ()
import Shared.Coordinate.Api.Resource.Coordinate.CoordinateSM ()
import Shared.DocumentTemplate.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.DocumentTemplate.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundlePackageSM ()
import Shared.KnowledgeModel.Api.Resource.KnowledgeModel.Bundle.KnowledgeModelBundleSM ()

type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

swagger :: Swagger
swagger =
  let s = toSwagger applicationApi
   in s
        { _swaggerInfo =
            s._swaggerInfo
              { _infoTitle = "Registry API"
              , _infoDescription = Just "API specification for Registry"
              , _infoVersion = "4.26.2"
              , _infoLicense =
                  Just $
                    License
                      { _licenseName = "Apache-2.0"
                      , _licenseUrl = Just . URL $ "https://raw.githubusercontent.com/ds-wizard/engine-backend/main/LICENSE.md"
                      }
              }
        }

swaggerServer :: Server SwaggerAPI
swaggerServer = swaggerSchemaUIServer swagger
