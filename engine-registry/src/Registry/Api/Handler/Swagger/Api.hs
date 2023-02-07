module Registry.Api.Handler.Swagger.Api where

import Control.Lens
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
import Shared.Api.Resource.DocumentTemplate.DocumentTemplateSM ()
import Shared.Api.Resource.DocumentTemplateBundle.DocumentTemplateBundleSM ()
import Shared.Api.Resource.Info.InfoSM ()
import Shared.Api.Resource.Package.PackageSM ()

type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

swagger :: Swagger
swagger =
  toSwagger applicationApi & info . title .~ "Registry API" & info . description ?~ "API specification for Registry"

swaggerServer :: Server SwaggerAPI
swaggerServer = swaggerSchemaUIServer swagger
