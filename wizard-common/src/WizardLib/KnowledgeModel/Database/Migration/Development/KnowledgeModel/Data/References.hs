module WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.References where

import Shared.Common.Model.Common.MapEntry
import Shared.Common.Util.Uuid
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

km1_ch1_q2_r1' :: Reference
km1_ch1_q2_r1' = ResourcePageReference' km1_ch1_q2_r1

km1_ch1_q2_r1 :: ResourcePageReference
km1_ch1_q2_r1 =
  ResourcePageReference
    { uuid = u' "903d0f50-040c-420e-9a65-49ba20ec6493"
    , shortUuid = "bvq"
    , annotations = []
    }

km1_ch1_q2_r1Edited' :: Reference
km1_ch1_q2_r1Edited' = ResourcePageReference' km1_ch1_q2_r1Edited

km1_ch1_q2_r1Edited :: ResourcePageReference
km1_ch1_q2_r1Edited =
  ResourcePageReference
    { uuid = km1_ch1_q2_r1.uuid
    , shortUuid = "bbb"
    , annotations = [MapEntry "newAnnotation" "someValue"]
    }

km1_ch1_q2_r1WithNewType' :: Reference
km1_ch1_q2_r1WithNewType' = URLReference' km1_ch1_q2_r1WithNewType

km1_ch1_q2_r1WithNewType :: URLReference
km1_ch1_q2_r1WithNewType =
  URLReference
    { uuid = km1_ch1_q2_r1.uuid
    , url = "https://wizard.org/dmp"
    , aLabel = "DMP Guide"
    , annotations = []
    }

km1_ch2_q6_r1' :: Reference
km1_ch2_q6_r1' = ResourcePageReference' km1_ch2_q6_r1

km1_ch2_q6_r1 :: ResourcePageReference
km1_ch2_q6_r1 =
  ResourcePageReference
    { uuid = u' "832ed9f5-107c-46e4-a13b-bf68086fcba1"
    , shortUuid = "bvq"
    , annotations = []
    }

-- ---------------------------------------------------------------------------
km1_ch1_q2_r2' :: Reference
km1_ch1_q2_r2' = URLReference' km1_ch1_q2_r2

km1_ch1_q2_r2 :: URLReference
km1_ch1_q2_r2 =
  URLReference
    { uuid = u' "fc379161-540e-47fb-8547-0504d4a397bf"
    , url = "https://wizard.org/fair"
    , aLabel = "F.A.I.R Principles"
    , annotations = []
    }

km1_ch1_q2_r2Edited' :: Reference
km1_ch1_q2_r2Edited' = URLReference' km1_ch1_q2_r2Edited

km1_ch1_q2_r2Edited :: URLReference
km1_ch1_q2_r2Edited =
  URLReference
    { uuid = km1_ch1_q2_r2.uuid
    , url = "EDITED: " ++ km1_ch1_q2_r2.url
    , aLabel = "EDITED: " ++ km1_ch1_q2_r2.aLabel
    , annotations = [MapEntry "newAnnotation" "someValue"]
    }

km1_ch1_q2_r2WithNewType' :: Reference
km1_ch1_q2_r2WithNewType' = CrossReference' km1_ch1_q2_r2WithNewType

km1_ch1_q2_r2WithNewType :: CrossReference
km1_ch1_q2_r2WithNewType =
  CrossReference
    { uuid = km1_ch1_q2_r2.uuid
    , targetUuid = u' "9d109b01-ca61-4a6b-9906-22ad4ffc057b"
    , description = "Link to my target"
    , annotations = []
    }

km1_ch2_q6_r2' :: Reference
km1_ch2_q6_r2' = URLReference' km1_ch2_q6_r2

km1_ch2_q6_r2 :: URLReference
km1_ch2_q6_r2 =
  URLReference
    { uuid = u' "29f973c8-1ec0-474a-8be5-84814c001496"
    , url = "https://wizard.org/fair"
    , aLabel = "F.A.I.R Principles"
    , annotations = []
    }

-- ---------------------------------------------------------------------------
km1_ch1_q2_r3' :: Reference
km1_ch1_q2_r3' = CrossReference' km1_ch1_q2_r3

km1_ch1_q2_r3 :: CrossReference
km1_ch1_q2_r3 =
  CrossReference
    { uuid = u' "d032ac2e-f58b-4c4b-87a4-8fbd45f155fa"
    , targetUuid = u' "4ced8015-82ae-4cf9-952d-9730a84a825a"
    , description = "Some description"
    , annotations = []
    }

km1_ch1_q2_r3Edited' :: Reference
km1_ch1_q2_r3Edited' = CrossReference' km1_ch1_q2_r3Edited

km1_ch1_q2_r3Edited :: CrossReference
km1_ch1_q2_r3Edited =
  CrossReference
    { uuid = km1_ch1_q2_r3.uuid
    , targetUuid = u' "bfe0a3bc-ee9f-45b7-98a7-7462cf0dd914"
    , description = "EDITED: " ++ km1_ch1_q2_r3.description
    , annotations = [MapEntry "newAnnotation" "someValue"]
    }

km1_ch1_q2_r3WithNewType' :: Reference
km1_ch1_q2_r3WithNewType' = ResourcePageReference' km1_ch1_q2_r3WithNewType

km1_ch1_q2_r3WithNewType :: ResourcePageReference
km1_ch1_q2_r3WithNewType =
  ResourcePageReference
    { uuid = km1_ch1_q2_r3.uuid
    , shortUuid = "awp"
    , annotations = []
    }

-- ---------------------------------------------------------------------------
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r1' :: Reference
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r1' = ResourcePageReference' km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r1

km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r1 :: ResourcePageReference
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r1 =
  ResourcePageReference
    { uuid = u' "994c2c75-4305-49bf-b207-c0d6f8042eb2"
    , shortUuid = "bvq"
    , annotations = []
    }

km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r2' :: Reference
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r2' = URLReference' km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r2

km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r2 :: URLReference
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r2 =
  URLReference
    { uuid = u' "931caf0b-a6ce-4183-8a02-7b02c2ff1e6c"
    , url = "https://wizard.org/fair"
    , aLabel = "F.A.I.R Principles"
    , annotations = []
    }
