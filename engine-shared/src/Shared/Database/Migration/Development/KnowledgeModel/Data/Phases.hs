module Shared.Database.Migration.Development.KnowledgeModel.Data.Phases where

import Shared.Model.Common.MapEntry
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Util.Uuid

phase1 :: Phase
phase1 =
  Phase
    { _phaseUuid = u' "b101f2d0-2476-452d-aa8d-95a41a02b52c"
    , _phaseTitle = "Before Submitting the Proposal"
    , _phaseDescription = Nothing
    , _phaseAnnotations = []
    }

phase1Edited :: Phase
phase1Edited =
  phase1
    { _phaseTitle = "EDITED: Before Submitting the Proposal"
    , _phaseDescription = Just "EDITED: some description"
    , _phaseAnnotations = [MapEntry "newAnnotation" "someValue"]
    }

phase2 :: Phase
phase2 =
  Phase
    { _phaseUuid = u' "1796fa3c-9f53-475f-89ff-c66a0453c42e"
    , _phaseTitle = "Before Submitting the DMP"
    , _phaseDescription = Nothing
    , _phaseAnnotations = []
    }

phase3 :: Phase
phase3 =
  Phase
    { _phaseUuid = u' "adc9133d-afcd-4616-9aea-db5f475898a2"
    , _phaseTitle = "Before Finishing the Project"
    , _phaseDescription = Nothing
    , _phaseAnnotations = []
    }

phase4 :: Phase
phase4 =
  Phase
    { _phaseUuid = u' "1ace0fc6-a949-495f-a32e-e948f3f6bed1"
    , _phaseTitle = "After Finishing the Project"
    , _phaseDescription = Nothing
    , _phaseAnnotations = []
    }
