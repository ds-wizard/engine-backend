module Wizard.Service.KnowledgeModel.Compilator.Modifier.Integration where

import Control.Lens ((^.))

import LensesConfig
import Shared.Model.Event.Integration.IntegrationEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddIntegrationEvent Integration where
  createEntity e =
    Integration
      { _integrationUuid = e ^. entityUuid
      , _integrationIId = e ^. iId
      , _integrationName = e ^. name
      , _integrationProps = e ^. props
      , _integrationLogo = e ^. logo
      , _integrationRequestMethod = e ^. requestMethod
      , _integrationRequestUrl = e ^. requestUrl
      , _integrationRequestHeaders = e ^. requestHeaders
      , _integrationRequestBody = e ^. requestBody
      , _integrationResponseListField = e ^. responseListField
      , _integrationResponseItemUrl = e ^. responseItemUrl
      , _integrationResponseItemId = e ^. responseItemId
      , _integrationResponseItemTemplate = e ^. responseItemTemplate
      , _integrationResponseExampleJson = e ^. responseExampleJson
      , _integrationAnnotations = e ^. annotations
      }

instance EditEntity EditIntegrationEvent Integration where
  editEntity e =
    applyIId .
    applyName .
    applyProps .
    applyLogo .
    applyRequestMethod .
    applyRequestUrl .
    applyRequestHeaders .
    applyRequestBody .
    applyResponseListField .
    applyResponseItemUrl . applyResponseItemId . applyResponseItemTemplate . applyResponseExampleJson . applyAnnotations
    where
      applyIId integration = applyValue (e ^. iId) integration iId
      applyName integration = applyValue (e ^. name) integration name
      applyProps integration = applyValue (e ^. props) integration props
      applyLogo integration = applyValue (e ^. logo) integration logo
      applyRequestMethod integration = applyValue (e ^. requestMethod) integration requestMethod
      applyRequestUrl integration = applyValue (e ^. requestUrl) integration requestUrl
      applyRequestHeaders integration = applyValue (e ^. requestHeaders) integration requestHeaders
      applyRequestBody integration = applyValue (e ^. requestBody) integration requestBody
      applyResponseListField integration = applyValue (e ^. responseListField) integration responseListField
      applyResponseItemUrl integration = applyValue (e ^. responseItemUrl) integration responseItemUrl
      applyResponseItemId integration = applyValue (e ^. responseItemId) integration responseItemId
      applyResponseItemTemplate integration = applyValue (e ^. responseItemTemplate) integration responseItemTemplate
      applyResponseExampleJson integration = applyValue (e ^. responseExampleJson) integration responseExampleJson
      applyAnnotations integration = applyValue (e ^. annotations) integration annotations
