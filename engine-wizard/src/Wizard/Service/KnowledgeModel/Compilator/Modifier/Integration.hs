module Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Integration.IntegrationEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddIntegrationEvent Integration where
  createEntity (AddApiIntegrationEvent' e) =
    ApiIntegration' $
    ApiIntegration
      { _apiIntegrationUuid = e ^. entityUuid
      , _apiIntegrationIId = e ^. iId
      , _apiIntegrationName = e ^. name
      , _apiIntegrationProps = e ^. props
      , _apiIntegrationLogo = e ^. logo
      , _apiIntegrationRequestMethod = e ^. requestMethod
      , _apiIntegrationRequestUrl = e ^. requestUrl
      , _apiIntegrationRequestHeaders = e ^. requestHeaders
      , _apiIntegrationRequestBody = e ^. requestBody
      , _apiIntegrationRequestEmptySearch = e ^. requestEmptySearch
      , _apiIntegrationResponseListField = e ^. responseListField
      , _apiIntegrationResponseItemId = e ^. responseItemId
      , _apiIntegrationResponseItemTemplate = e ^. responseItemTemplate
      , _apiIntegrationItemUrl = e ^. itemUrl
      , _apiIntegrationAnnotations = e ^. annotations
      }
  createEntity (AddWidgetIntegrationEvent' e) =
    WidgetIntegration' $
    WidgetIntegration
      { _widgetIntegrationUuid = e ^. entityUuid
      , _widgetIntegrationIId = e ^. iId
      , _widgetIntegrationName = e ^. name
      , _widgetIntegrationProps = e ^. props
      , _widgetIntegrationLogo = e ^. logo
      , _widgetIntegrationWidgetUrl = e ^. widgetUrl
      , _widgetIntegrationItemUrl = e ^. itemUrl
      , _widgetIntegrationAnnotations = e ^. annotations
      }

instance EditEntity EditIntegrationEvent Integration where
  editEntity e' integration =
    case e' of
      (EditApiIntegrationEvent' e) -> ApiIntegration' . applyToApiIntegration e . convertToApiIntegration $ integration
      (EditWidgetIntegrationEvent' e) ->
        WidgetIntegration' . applyToWidgetIntegration e . convertToWidgetIntegration $ integration
    where
      applyToApiIntegration e =
        applyIId e .
        applyName e .
        applyProps e .
        applyLogo e .
        applyRequestMethod e .
        applyRequestUrl e .
        applyRequestHeaders e .
        applyRequestBody e .
        applyRequestEmptySearch e .
        applyResponseListField e .
        applyResponseItemId e . applyResponseItemTemplate e . applyItemUrl e . applyAnnotations e
      applyToWidgetIntegration e =
        applyIId e . applyName e . applyProps e . applyLogo e . applyWidgetUrl e . applyItemUrl e . applyAnnotations e
      applyIId e integration = applyValue (e ^. iId) integration iId
      applyName e integration = applyValue (e ^. name) integration name
      applyProps e integration = applyValue (e ^. props) integration props
      applyLogo e integration = applyValue (e ^. logo) integration logo
      applyRequestMethod e integration = applyValue (e ^. requestMethod) integration requestMethod
      applyRequestUrl e integration = applyValue (e ^. requestUrl) integration requestUrl
      applyRequestHeaders e integration = applyValue (e ^. requestHeaders) integration requestHeaders
      applyRequestBody e integration = applyValue (e ^. requestBody) integration requestBody
      applyRequestEmptySearch e integration = applyValue (e ^. requestEmptySearch) integration requestEmptySearch
      applyResponseListField e integration = applyValue (e ^. responseListField) integration responseListField
      applyResponseItemId e integration = applyValue (e ^. responseItemId) integration responseItemId
      applyResponseItemTemplate e integration = applyValue (e ^. responseItemTemplate) integration responseItemTemplate
      applyItemUrl e integration = applyValue (e ^. itemUrl) integration itemUrl
      applyAnnotations e integration = applyValue (e ^. annotations) integration annotations
      applyWidgetUrl e integration = applyValue (e ^. widgetUrl) integration widgetUrl

convertToApiIntegration :: Integration -> ApiIntegration
convertToApiIntegration (ApiIntegration' integration) = integration
convertToApiIntegration integration' =
  case integration' of
    (WidgetIntegration' integration) -> createIntegration integration
  where
    createIntegration integration =
      ApiIntegration
        { _apiIntegrationUuid = integration ^. uuid
        , _apiIntegrationIId = integration ^. iId
        , _apiIntegrationName = integration ^. name
        , _apiIntegrationProps = integration ^. props
        , _apiIntegrationLogo = integration ^. logo
        , _apiIntegrationRequestMethod = ""
        , _apiIntegrationRequestUrl = integration ^. widgetUrl
        , _apiIntegrationRequestHeaders = []
        , _apiIntegrationRequestBody = ""
        , _apiIntegrationRequestEmptySearch = True
        , _apiIntegrationResponseListField = ""
        , _apiIntegrationResponseItemId = ""
        , _apiIntegrationResponseItemTemplate = ""
        , _apiIntegrationItemUrl = integration ^. itemUrl
        , _apiIntegrationAnnotations = integration ^. annotations
        }

convertToWidgetIntegration :: Integration -> WidgetIntegration
convertToWidgetIntegration (WidgetIntegration' integration) = integration
convertToWidgetIntegration integration' =
  case integration' of
    (ApiIntegration' integration) -> createIntegration integration
  where
    createIntegration integration =
      WidgetIntegration
        { _widgetIntegrationUuid = integration ^. uuid
        , _widgetIntegrationIId = integration ^. iId
        , _widgetIntegrationName = integration ^. name
        , _widgetIntegrationProps = integration ^. props
        , _widgetIntegrationLogo = integration ^. logo
        , _widgetIntegrationWidgetUrl = integration ^. requestUrl
        , _widgetIntegrationItemUrl = integration ^. itemUrl
        , _widgetIntegrationAnnotations = integration ^. annotations
        }
