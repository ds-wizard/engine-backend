module Wizard.Specs.Service.KnowledgeModel.Metamodel.Migrator.Migration0019.MigrationFixtures where

import Data.Aeson.QQ

addIntegrationEventIn1 =
  [aesonQQ|
    {
      "uuid": "00000000-0000-0000-0000-000000000000",
      "parentUuid": "00000000-0000-0000-0000-000000000000",
      "entityUuid": "00000000-0000-0000-0000-000000000000",
      "content": {
        "id": "ID",
        "logo": "",
        "name": "Name",
        "itemUrl": "",
        "eventType": "AddIntegrationEvent",
        "variables": [],
        "requestUrl": "",
        "annotations": [],
        "requestBody": "",
        "requestMethod": "GET",
        "requestHeaders": [],
        "responseItemId": "",
        "integrationType": "ApiLegacyIntegration",
        "responseListField": "",
        "requestEmptySearch": true,
        "responseItemTemplate": ""
      },
      "createdAt": "2024-06-01T00:00:00Z"
    }
  |]

addIntegrationEventOut1 =
  [aesonQQ|
    {
      "uuid": "00000000-0000-0000-0000-000000000000",
      "parentUuid": "00000000-0000-0000-0000-000000000000",
      "entityUuid": "00000000-0000-0000-0000-000000000000",
      "content": {
        "name": "Name",
        "testQ": "",
        "eventType": "AddIntegrationEvent",
        "variables": [],
        "requestUrl": "",
        "annotations": [],
        "requestBody": "",
        "testResponse": null,
        "requestMethod": "GET",
        "testVariables": {},
        "requestHeaders": [],
        "integrationType": "ApiIntegration",
        "allowCustomReply": true,
        "responseListField": "",
        "responseItemTemplate": "",
        "requestAllowEmptySearch": true,
        "responseItemTemplateForSelection": null
      },
      "createdAt": "2024-06-01T00:00:00Z"
    }
  |]

editIntegrationEventIn1 =
  [aesonQQ|
    {
      "uuid": "00000000-0000-0000-0000-000000000000",
      "parentUuid": "00000000-0000-0000-0000-000000000000",
      "entityUuid": "00000000-0000-0000-0000-000000000000",
      "content": {
        "id": {
          "changed": false
        },
        "logo": {
          "changed": false
        },
        "name": {
          "changed": true,
          "value": "New Name"
        },
        "itemUrl": {
          "changed": false
        },
        "eventType": "EditIntegrationEvent",
        "variables": {
          "changed": false
        },
        "requestUrl": {
          "changed": false
        },
        "annotations": {
          "changed": false
        },
        "requestBody": {
          "changed": false
        },
        "requestMethod": {
          "changed": false
        },
        "requestHeaders": {
          "changed": false
        },
        "responseItemId": {
          "changed": false
        },
        "integrationType": "ApiLegacyIntegration",
        "responseListField": {
          "changed": false
        },
        "requestEmptySearch": {
          "changed": false
        },
        "responseItemTemplate": {
          "changed": false
        }
      },
      "createdAt": "2024-06-01T00:00:00Z"
    }
  |]

editIntegrationEventOut1 =
  [aesonQQ|
    {
      "uuid": "00000000-0000-0000-0000-000000000000",
      "parentUuid": "00000000-0000-0000-0000-000000000000",
      "entityUuid": "00000000-0000-0000-0000-000000000000",
      "content": {
        "name": {
          "value": "New Name",
          "changed": true
        },
        "testQ": {
          "changed": false
        },
        "eventType": "EditIntegrationEvent",
        "variables": {
          "changed": false
        },
        "requestUrl": {
          "changed": false
        },
        "annotations": {
          "changed": false
        },
        "requestBody": {
          "changed": false
        },
        "testResponse": {
          "changed": false
        },
        "requestMethod": {
          "changed": false
        },
        "testVariables": {
          "changed": false
        },
        "requestHeaders": {
          "changed": false
        },
        "integrationType": "ApiIntegration",
        "allowCustomReply": {
          "changed": false
        },
        "responseListField": {
          "changed": false
        },
        "responseItemTemplate": {
          "changed": false
        },
        "requestAllowEmptySearch": {
          "changed": false
        },
        "responseItemTemplateForSelection": {
          "changed": false
        }
      },
      "createdAt": "2024-06-01T00:00:00Z"
    }
  |]

addIntegrationEventIn2 =
  [aesonQQ|
    {
      "uuid": "00000000-0000-0000-0000-000000000000",
      "parentUuid": "00000000-0000-0000-0000-000000000000",
      "entityUuid": "00000000-0000-0000-0000-000000000000",
      "content": {
        "id": "ID",
        "logo": "",
        "name": "None",
        "itemUrl": "",
        "eventType": "AddIntegrationEvent",
        "variables": [],
        "widgetUrl": "",
        "annotations": [],
        "integrationType": "WidgetIntegration"
      },
      "createdAt": "2024-06-01T00:00:00Z"
    }
  |]

editIntegrationEventIn2 =
  [aesonQQ|
    {
      "uuid": "00000000-0000-0000-0000-000000000000",
      "parentUuid": "00000000-0000-0000-0000-000000000000",
      "entityUuid": "00000000-0000-0000-0000-000000000000",
      "content": {
        "id": {
          "value": "Some ID",
          "changed": true
        },
        "logo": {
          "changed": false
        },
        "name": {
          "changed": false
        },
        "itemUrl": {
          "changed": false
        },
        "eventType": "EditIntegrationEvent",
        "variables": {
          "changed": false
        },
        "widgetUrl": {
          "changed": false
        },
        "annotations": {
          "changed": false
        },
        "integrationType": "WidgetIntegration"
      },
      "createdAt": "2024-06-01T00:00:00Z"
    }
  |]

addIntegrationEventIn3 =
  [aesonQQ|
    {
      "uuid": "00000000-0000-0000-0000-000000000000",
      "parentUuid": "00000000-0000-0000-0000-000000000000",
      "entityUuid": "00000000-0000-0000-0000-000000000000",
      "content": {
        "name": "",
        "testQ": "",
        "eventType": "AddIntegrationEvent",
        "variables": [],
        "requestUrl": "",
        "annotations": [],
        "requestBody": null,
        "testResponse": null,
        "requestMethod": "GET",
        "testVariables": {},
        "requestHeaders": [
          {
            "key": "Accept",
            "value": "application/json"
          }
        ],
        "integrationType": "ApiIntegration",
        "allowCustomReply": true,
        "responseListField": null,
        "responseItemTemplate": "",
        "requestAllowEmptySearch": true,
        "responseItemTemplateForSelection": null
      },
      "createdAt": "2024-06-01T00:00:00Z"
    }
  |]

editIntegrationEventIn3 =
  [aesonQQ|
    {
      "uuid": "00000000-0000-0000-0000-000000000000",
      "parentUuid": "00000000-0000-0000-0000-000000000000",
      "entityUuid": "00000000-0000-0000-0000-000000000000",
      "content": {
        "name": {
          "value": "New Name",
          "changed": true
        },
        "testQ": {
          "changed": false
        },
        "eventType": "EditIntegrationEvent",
        "variables": {
          "changed": false
        },
        "requestUrl": {
          "changed": false
        },
        "annotations": {
          "changed": false
        },
        "requestBody": {
          "changed": false
        },
        "testResponse": {
          "changed": false
        },
        "requestMethod": {
          "changed": false
        },
        "testVariables": {
          "changed": false
        },
        "requestHeaders": {
          "changed": false
        },
        "integrationType": "ApiIntegration",
        "allowCustomReply": {
          "changed": false
        },
        "responseListField": {
          "changed": false
        },
        "responseItemTemplate": {
          "changed": false
        },
        "requestAllowEmptySearch": {
          "changed": false
        },
        "responseItemTemplateForSelection": {
          "changed": false
        }
      },
      "createdAt": "2024-06-01T00:00:00Z"
    }
  |]
