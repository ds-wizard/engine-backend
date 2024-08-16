module WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Resources where

import Shared.Common.Model.Common.MapEntry
import Shared.Common.Util.Uuid
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

rc1 :: ResourceCollection
rc1 =
  ResourceCollection
    { uuid = u' "46de8ffd-f798-4a23-a548-923405a6747b"
    , title = "Resource Collection 1"
    , resourcePageUuids = [rc1_rp1.uuid, rc1_rp2.uuid]
    , annotations = [MapEntry "newAnnotation1" "someValue1"]
    }

rc1Edited :: ResourceCollection
rc1Edited =
  ResourceCollection
    { uuid = rc1.uuid
    , title = "EDITED: Resource Collection 1"
    , resourcePageUuids = [rc1_rp2.uuid, rc1_rp1.uuid]
    , annotations = [MapEntry "newAnnotation1Edited" "someValue1Edited"]
    }

rc1_rp1 :: ResourcePage
rc1_rp1 =
  ResourcePage
    { uuid = u' "ea4ed8f1-9b04-4f39-8260-9fabd505b0b7"
    , title = "Resource Page 1.1"
    , content = "Some content 1.1"
    , annotations = [MapEntry "newAnnotation1.1" "someValue1.1"]
    }

rc1_rp1Edited :: ResourcePage
rc1_rp1Edited =
  ResourcePage
    { uuid = rc1_rp1.uuid
    , title = "EDITED: Resource Page 1.1"
    , content = "EDITED: Some content 1.1"
    , annotations = [MapEntry "newAnnotation1.1Edited" "someValue1.1Edited"]
    }

rc1_rp2 :: ResourcePage
rc1_rp2 =
  ResourcePage
    { uuid = u' "e563a9cd-0f1c-4f96-ac93-ae602165db0f"
    , title = "Resource Page 1.2"
    , content = "Some content 1.2"
    , annotations = [MapEntry "newAnnotation1.2" "someValue1.2"]
    }

rc2 :: ResourceCollection
rc2 =
  ResourceCollection
    { uuid = u' "b8f66287-cb43-42d3-8b55-d8ee83ff5189"
    , title = "Resource Collection 2"
    , resourcePageUuids = [rc2_rp1.uuid]
    , annotations = [MapEntry "newAnnotation2" "someValue2"]
    }

rc2_rp1 :: ResourcePage
rc2_rp1 =
  ResourcePage
    { uuid = u' "15490ee5-8434-417b-ba6c-35bf7028a1f4"
    , title = "Resource Page 2.1"
    , content = "Some content 2.1"
    , annotations = [MapEntry "newAnnotation2.1" "someValue2.1"]
    }
