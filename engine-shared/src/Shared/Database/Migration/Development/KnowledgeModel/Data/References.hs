module Shared.Database.Migration.Development.KnowledgeModel.Data.References where

import Control.Lens
import qualified Data.Map.Strict as M

import LensesConfig
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Uuid

km1_ch1_q2_r1' :: Reference
km1_ch1_q2_r1' = ResourcePageReference' km1_ch1_q2_r1

km1_ch1_q2_r1 :: ResourcePageReference
km1_ch1_q2_r1 =
  ResourcePageReference
    { _resourcePageReferenceUuid = u' "903d0f50-040c-420e-9a65-49ba20ec6493"
    , _resourcePageReferenceShortUuid = "bvq"
    , _resourcePageReferenceAnnotations = M.empty
    }

km1_ch1_q2_r1Edited' :: Reference
km1_ch1_q2_r1Edited' = ResourcePageReference' km1_ch1_q2_r1Edited

km1_ch1_q2_r1Edited :: ResourcePageReference
km1_ch1_q2_r1Edited =
  ResourcePageReference
    { _resourcePageReferenceUuid = km1_ch1_q2_r1 ^. uuid
    , _resourcePageReferenceShortUuid = "bbb"
    , _resourcePageReferenceAnnotations = M.fromList [("newAnnotation", "someValue")]
    }

km1_ch1_q2_r1WithNewType' :: Reference
km1_ch1_q2_r1WithNewType' = URLReference' km1_ch1_q2_r1WithNewType

km1_ch1_q2_r1WithNewType :: URLReference
km1_ch1_q2_r1WithNewType =
  URLReference
    { _uRLReferenceUuid = km1_ch1_q2_r1 ^. uuid
    , _uRLReferenceUrl = "https://wizard.org/dmp"
    , _uRLReferenceLabel = "DMP Guide"
    , _uRLReferenceAnnotations = M.empty
    }

km1_ch2_q6_r1' :: Reference
km1_ch2_q6_r1' = ResourcePageReference' km1_ch2_q6_r1

km1_ch2_q6_r1 :: ResourcePageReference
km1_ch2_q6_r1 =
  ResourcePageReference
    { _resourcePageReferenceUuid = u' "832ed9f5-107c-46e4-a13b-bf68086fcba1"
    , _resourcePageReferenceShortUuid = "bvq"
    , _resourcePageReferenceAnnotations = M.empty
    }

-- ---------------------------------------------------------------------------
km1_ch1_q2_r2' :: Reference
km1_ch1_q2_r2' = URLReference' km1_ch1_q2_r2

km1_ch1_q2_r2 :: URLReference
km1_ch1_q2_r2 =
  URLReference
    { _uRLReferenceUuid = u' "fc379161-540e-47fb-8547-0504d4a397bf"
    , _uRLReferenceUrl = "https://wizard.org/fair"
    , _uRLReferenceLabel = "F.A.I.R Principles"
    , _uRLReferenceAnnotations = M.empty
    }

km1_ch1_q2_r2Edited' :: Reference
km1_ch1_q2_r2Edited' = URLReference' km1_ch1_q2_r2Edited

km1_ch1_q2_r2Edited :: URLReference
km1_ch1_q2_r2Edited =
  URLReference
    { _uRLReferenceUuid = km1_ch1_q2_r2 ^. uuid
    , _uRLReferenceUrl = "EDITED: " ++ km1_ch1_q2_r2 ^. url
    , _uRLReferenceLabel = "EDITED: " ++ km1_ch1_q2_r2 ^. label
    , _uRLReferenceAnnotations = M.fromList [("newAnnotation", "someValue")]
    }

km1_ch1_q2_r2WithNewType' :: Reference
km1_ch1_q2_r2WithNewType' = CrossReference' km1_ch1_q2_r2WithNewType

km1_ch1_q2_r2WithNewType :: CrossReference
km1_ch1_q2_r2WithNewType =
  CrossReference
    { _crossReferenceUuid = km1_ch1_q2_r2 ^. uuid
    , _crossReferenceTargetUuid = u' "9d109b01-ca61-4a6b-9906-22ad4ffc057b"
    , _crossReferenceDescription = "Link to my target"
    , _crossReferenceAnnotations = M.empty
    }

km1_ch2_q6_r2' :: Reference
km1_ch2_q6_r2' = URLReference' km1_ch2_q6_r2

km1_ch2_q6_r2 :: URLReference
km1_ch2_q6_r2 =
  URLReference
    { _uRLReferenceUuid = u' "29f973c8-1ec0-474a-8be5-84814c001496"
    , _uRLReferenceUrl = "https://wizard.org/fair"
    , _uRLReferenceLabel = "F.A.I.R Principles"
    , _uRLReferenceAnnotations = M.empty
    }

-- ---------------------------------------------------------------------------
km1_ch1_q2_r3' :: Reference
km1_ch1_q2_r3' = CrossReference' km1_ch1_q2_r3

km1_ch1_q2_r3 :: CrossReference
km1_ch1_q2_r3 =
  CrossReference
    { _crossReferenceUuid = u' "d032ac2e-f58b-4c4b-87a4-8fbd45f155fa"
    , _crossReferenceTargetUuid = u' "4ced8015-82ae-4cf9-952d-9730a84a825a"
    , _crossReferenceDescription = "Some description"
    , _crossReferenceAnnotations = M.empty
    }

km1_ch1_q2_r3Edited' :: Reference
km1_ch1_q2_r3Edited' = CrossReference' km1_ch1_q2_r3Edited

km1_ch1_q2_r3Edited :: CrossReference
km1_ch1_q2_r3Edited =
  CrossReference
    { _crossReferenceUuid = km1_ch1_q2_r3 ^. uuid
    , _crossReferenceTargetUuid = u' "bfe0a3bc-ee9f-45b7-98a7-7462cf0dd914"
    , _crossReferenceDescription = "EDITED: " ++ km1_ch1_q2_r3 ^. description
    , _crossReferenceAnnotations = M.fromList [("newAnnotation", "someValue")]
    }

km1_ch1_q2_r3WithNewType' :: Reference
km1_ch1_q2_r3WithNewType' = ResourcePageReference' km1_ch1_q2_r3WithNewType

km1_ch1_q2_r3WithNewType :: ResourcePageReference
km1_ch1_q2_r3WithNewType =
  ResourcePageReference
    { _resourcePageReferenceUuid = km1_ch1_q2_r3 ^. uuid
    , _resourcePageReferenceShortUuid = "awp"
    , _resourcePageReferenceAnnotations = M.empty
    }

-- ---------------------------------------------------------------------------
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r1' :: Reference
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r1' = ResourcePageReference' km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r1

km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r1 :: ResourcePageReference
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r1 =
  ResourcePageReference
    { _resourcePageReferenceUuid = u' "994c2c75-4305-49bf-b207-c0d6f8042eb2"
    , _resourcePageReferenceShortUuid = "bvq"
    , _resourcePageReferenceAnnotations = M.empty
    }

km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r2' :: Reference
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r2' = URLReference' km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r2

km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r2 :: URLReference
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_r2 =
  URLReference
    { _uRLReferenceUuid = u' "931caf0b-a6ce-4183-8a02-7b02c2ff1e6c"
    , _uRLReferenceUrl = "https://wizard.org/fair"
    , _uRLReferenceLabel = "F.A.I.R Principles"
    , _uRLReferenceAnnotations = M.empty
    }
