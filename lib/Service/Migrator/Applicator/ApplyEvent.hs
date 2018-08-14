module Service.Migrator.Applicator.ApplyEvent where

import Model.Error.Error
import Model.Event.EventPath
import Model.KnowledgeModel.KnowledgeModel

class ApplyEventToKM e where
  applyEventToKM :: e -> EventPath -> Either AppError (Maybe KnowledgeModel) -> Either AppError (Maybe KnowledgeModel)

class ApplyEventToChapter e where
  applyEventToChapter :: e -> EventPath -> Either AppError Chapter -> Either AppError Chapter

class ApplyEventToQuestion e where
  applyEventToQuestion :: e -> EventPath -> Either AppError Question -> Either AppError Question

class ApplyEventToAnswer e where
  applyEventToAnswer :: e -> EventPath -> Either AppError Answer -> Either AppError Answer

class ApplyEventToExpert e where
  applyEventToExpert :: e -> EventPath -> Either AppError Expert -> Either AppError Expert

class ApplyEventToReference e where
  applyEventToReference :: e -> EventPath -> Either AppError Reference -> Either AppError Reference

-- --------------------------------
-- HELPERS
-- --------------------------------
heApplyEventToKM e path eKM callback =
  case applyEventToKM e path eKM of
    Right km -> callback km
    Left e -> Left e

heApplyEventToChapter e path eChapter callback =
  case applyEventToChapter e path eChapter of
    Right chapter -> callback chapter
    Left e -> Left e

heApplyEventToQuestion e path eQuestion callback =
  case applyEventToQuestion e path eQuestion of
    Right question -> callback question
    Left e -> Left e

heApplyEventToAnswer e path eAnswer callback =
  case applyEventToAnswer e path eAnswer of
    Right answer -> callback answer
    Left e -> Left e

heApplyEventToExpert e path eExpert callback =
  case applyEventToExpert e path eExpert of
    Right expert -> callback expert
    Left e -> Left e

heApplyEventToReference e path eReference callback =
  case applyEventToReference e path eReference of
    Right reference -> callback reference
    Left e -> Left e
