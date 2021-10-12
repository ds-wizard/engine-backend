module Shared.Database.Migration.Development.KnowledgeModel.Data.Experts where

import qualified Data.Map.Strict as M

import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Uuid

km1_ch1_q2_eAlbert :: Expert
km1_ch1_q2_eAlbert =
  Expert
    { _expertUuid = u' "04b3661a-4176-4d5e-954f-6827f1888b8f"
    , _expertName = "Albert Einstein"
    , _expertEmail = "albert.einstein@example.com"
    , _expertAnnotations = M.empty
    }

km1_ch1_q2_eAlbertEdited :: Expert
km1_ch1_q2_eAlbertEdited =
  km1_ch1_q2_eAlbert
    { _expertName = "EDITED: Albert Einstein"
    , _expertEmail = "EDITED: albert.einstein@example.com"
    , _expertAnnotations = M.fromList [("newAnnotation", "someValue")]
    }

km1_ch1_q2_eNikola :: Expert
km1_ch1_q2_eNikola =
  Expert
    { _expertUuid = u' "78597e39-628a-47fd-8b3d-cc149f1c53e9"
    , _expertName = "Nikola Tesla"
    , _expertEmail = "nikola.tesla@example.com"
    , _expertAnnotations = M.empty
    }

km1_ch1_q2_eIsaac :: Expert
km1_ch1_q2_eIsaac =
  Expert
    { _expertUuid = u' "e56a5fea-6e01-4898-8db0-741200073752"
    , _expertName = "Isaac Newton"
    , _expertEmail = "isaac.newton@example.com"
    , _expertAnnotations = M.empty
    }

-- ---------------------------------------------------------------------------
km1_ch2_q6_eAlbert :: Expert
km1_ch2_q6_eAlbert =
  Expert
    { _expertUuid = u' "6fbed760-a612-485f-8f0a-7d69b97a103a"
    , _expertName = "Albert Einstein"
    , _expertEmail = "albert.einstein@example.com"
    , _expertAnnotations = M.empty
    }

km1_ch2_q6_eNikola :: Expert
km1_ch2_q6_eNikola =
  Expert
    { _expertUuid = u' "0d545857-179a-49e7-ac98-934b91a53e93"
    , _expertName = "Nikola Tesla"
    , _expertEmail = "nikola.tesla@example.com"
    , _expertAnnotations = M.empty
    }

-- ---------------------------------------------------------------------------
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_eAlbert :: Expert
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_eAlbert =
  Expert
    { _expertUuid = u' "14c3db17-923a-4c1c-8cf0-8d5bad682b3f"
    , _expertName = "Albert Einstein"
    , _expertEmail = "albert.einstein@example.com"
    , _expertAnnotations = M.empty
    }

km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_eNikola :: Expert
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_eNikola =
  Expert
    { _expertUuid = u' "b8560122-a5ad-4742-9d8a-331b117cb831"
    , _expertName = "Nikola Tesla"
    , _expertEmail = "nikola.tesla@example.com"
    , _expertAnnotations = M.empty
    }
