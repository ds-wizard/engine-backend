module WizardLib.Common.Util.Hashable where

import Data.Hashable

import Shared.Common.Model.Common.MapEntry
import Shared.Common.Util.Hashable ()
import WizardLib.KnowledgeModel.Model.Event.EventField
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel

instance Hashable MetricMeasure
instance Hashable QuestionValidation
instance Hashable QuestionValueType
instance Hashable a => Hashable (EventField a)
instance (Hashable key, Hashable value) => Hashable (MapEntry key value)
