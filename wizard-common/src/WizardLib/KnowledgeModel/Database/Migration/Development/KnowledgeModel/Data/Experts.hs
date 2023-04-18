module WizardLib.KnowledgeModel.Database.Migration.Development.KnowledgeModel.Data.Experts where

import Shared.Common.Model.Common.MapEntry
import Shared.Common.Util.Uuid
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

km1_ch1_q2_eAlbert :: Expert
km1_ch1_q2_eAlbert =
  Expert
    { uuid = u' "04b3661a-4176-4d5e-954f-6827f1888b8f"
    , name = "Albert Einstein"
    , email = "albert.einstein@example.com"
    , annotations = []
    }

km1_ch1_q2_eAlbertEdited :: Expert
km1_ch1_q2_eAlbertEdited =
  km1_ch1_q2_eAlbert
    { name = "EDITED: Albert Einstein"
    , email = "EDITED: albert.einstein@example.com"
    , annotations = [MapEntry "newAnnotation" "someValue"]
    }

km1_ch1_q2_eNikola :: Expert
km1_ch1_q2_eNikola =
  Expert
    { uuid = u' "78597e39-628a-47fd-8b3d-cc149f1c53e9"
    , name = "Nikola Tesla"
    , email = "nikola.tesla@example.com"
    , annotations = []
    }

km1_ch1_q2_eIsaac :: Expert
km1_ch1_q2_eIsaac =
  Expert
    { uuid = u' "e56a5fea-6e01-4898-8db0-741200073752"
    , name = "Isaac Newton"
    , email = "isaac.newton@example.com"
    , annotations = []
    }

-- ---------------------------------------------------------------------------
km1_ch2_q6_eAlbert :: Expert
km1_ch2_q6_eAlbert =
  Expert
    { uuid = u' "6fbed760-a612-485f-8f0a-7d69b97a103a"
    , name = "Albert Einstein"
    , email = "albert.einstein@example.com"
    , annotations = []
    }

km1_ch2_q6_eNikola :: Expert
km1_ch2_q6_eNikola =
  Expert
    { uuid = u' "0d545857-179a-49e7-ac98-934b91a53e93"
    , name = "Nikola Tesla"
    , email = "nikola.tesla@example.com"
    , annotations = []
    }

-- ---------------------------------------------------------------------------
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_eAlbert :: Expert
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_eAlbert =
  Expert
    { uuid = u' "14c3db17-923a-4c1c-8cf0-8d5bad682b3f"
    , name = "Albert Einstein"
    , email = "albert.einstein@example.com"
    , annotations = []
    }

km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_eNikola :: Expert
km1_ch1_ansYes1_fuq1_ansYes3_fuq2_2_eNikola =
  Expert
    { uuid = u' "b8560122-a5ad-4742-9d8a-331b117cb831"
    , name = "Nikola Tesla"
    , email = "nikola.tesla@example.com"
    , annotations = []
    }
