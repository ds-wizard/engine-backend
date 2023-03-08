module Registry.Api.Handler.Swagger.Api where

import Data.Swagger
import Servant
import Servant.Swagger
import Servant.Swagger.UI

import Registry.Api.Api
import Registry.Api.Resource.ActionKey.ActionKeySM ()
import Registry.Api.Resource.DocumentTemplate.DocumentTemplateDetailSM ()
import Registry.Api.Resource.DocumentTemplate.DocumentTemplateSimpleSM ()
import Registry.Api.Resource.Locale.LocaleDetailSM ()
import Registry.Api.Resource.Locale.LocaleSM ()
import Registry.Api.Resource.Organization.OrganizationChangeSM ()
import Registry.Api.Resource.Organization.OrganizationCreateSM ()
import Registry.Api.Resource.Organization.OrganizationSM ()
import Registry.Api.Resource.Organization.OrganizationStateSM ()
import Registry.Api.Resource.Package.PackageDetailSM ()
import Registry.Api.Resource.Package.PackageSimpleSM ()
import Registry.Api.Resource.PackageBundle.PackageBundleSM ()
import Shared.Api.Resource.Common.FileSM ()
import Shared.Api.Resource.Component.ComponentSM ()
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleSM ()
import Shared.Api.Resource.Info.InfoSM ()
import Shared.Api.Resource.Package.PackageSM ()
import Shared.Api.Resource.PackageBundle.PackageBundleSM ()

type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

swagger :: Swagger
swagger =
  let s = toSwagger applicationApi
   in s
        { _swaggerInfo =
            s._swaggerInfo
              { _infoTitle = "Registry API"
              , _infoDescription = Just "API specification for Registry"
              }
        }

swaggerServer :: Server SwaggerAPI
swaggerServer = swaggerSchemaUIServer swagger
