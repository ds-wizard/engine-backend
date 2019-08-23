module DSW.Metamodel.Migration.Migration3 where

import Data.Aeson
import Data.Either
import qualified Data.Map as M
import qualified Data.UUID as U

import qualified DSW.Metamodel.Event.Version3 as V3
import qualified DSW.Metamodel.Event.Version4 as V4

result2Either :: Result a -> Either String a
result2Either (Error msg) = Left msg
result2Either (Success x) = Right x

getParentUuid :: V3.EventPathDTO -> U.UUID
getParentUuid = last . map V3._eventPathItemDTOUuid

class Upgradeable f t where
  upgrade :: f -> Either String t

instance (Upgradeable f t) => Upgradeable [f] [t] where
  upgrade lst =
    case (partitionEithers $ map upgrade lst) of
      ([], r) -> Right r
      ((x:_), _) -> Left x

instance Upgradeable Char Char where
  upgrade = Right

instance Upgradeable U.UUID U.UUID where
  upgrade = Right

instance Upgradeable (Maybe Int) (Maybe Int) where
  upgrade = Right

instance Upgradeable (Maybe String) (Maybe String) where
  upgrade = Right

instance Upgradeable (M.Map String String) (M.Map String String) where
  upgrade = Right

instance (Upgradeable f t) => Upgradeable (V3.EventFieldDTO f) (V4.EventFieldDTO t) where
  upgrade V3.NothingChangedDTO = Right V4.NothingChangedDTO
  upgrade (V3.ChangedValueDTO x) = V4.ChangedValueDTO <$> upgrade x

instance Upgradeable V3.QuestionValueType V4.QuestionValueType where
  upgrade V3.StringQuestionValueType = Right V4.StringQuestionValueType
  upgrade V3.NumberQuestionValueType = Right V4.NumberQuestionValueType
  upgrade V3.DateQuestionValueType = Right V4.DateQuestionValueType
  upgrade V3.TextQuestionValueType = Right V4.TextQuestionValueType

instance Upgradeable V3.MetricMeasureDTO V4.MetricMeasureDTO where
  upgrade x = result2Either . fromJSON . toJSON $ x

instance Upgradeable V3.AddAnswerEventDTO V4.AddAnswerEventDTO where
  upgrade (V3.AddAnswerEventDTO {..}) =
    V4.AddAnswerEventDTO
      _addAnswerEventDTOUuid
      (getParentUuid _addAnswerEventDTOPath)
      _addAnswerEventDTOAnswerUuid
      _addAnswerEventDTOLabel
      _addAnswerEventDTOAdvice <$>
    upgrade _addAnswerEventDTOMetricMeasures

instance Upgradeable V3.EditAnswerEventDTO V4.EditAnswerEventDTO where
  upgrade (V3.EditAnswerEventDTO {..}) =
    V4.EditAnswerEventDTO _editAnswerEventDTOUuid (getParentUuid _editAnswerEventDTOPath) _editAnswerEventDTOAnswerUuid <$>
    upgrade _editAnswerEventDTOLabel <*>
    upgrade _editAnswerEventDTOAdvice <*>
    upgrade _editAnswerEventDTOFollowUpUuids <*>
    upgrade _editAnswerEventDTOMetricMeasures

instance Upgradeable V3.DeleteAnswerEventDTO V4.DeleteAnswerEventDTO where
  upgrade (V3.DeleteAnswerEventDTO {..}) =
    return $
    V4.DeleteAnswerEventDTO
      _deleteAnswerEventDTOUuid
      (getParentUuid _deleteAnswerEventDTOPath)
      _deleteAnswerEventDTOAnswerUuid

instance Upgradeable V3.AddChapterEventDTO V4.AddChapterEventDTO where
  upgrade (V3.AddChapterEventDTO {..}) =
    return $
    V4.AddChapterEventDTO
      _addChapterEventDTOUuid
      (getParentUuid _addChapterEventDTOPath)
      _addChapterEventDTOChapterUuid
      _addChapterEventDTOTitle
      _addChapterEventDTOText

instance Upgradeable V3.EditChapterEventDTO V4.EditChapterEventDTO where
  upgrade (V3.EditChapterEventDTO {..}) =
    V4.EditChapterEventDTO
      _editChapterEventDTOUuid
      (getParentUuid _editChapterEventDTOPath)
      _editChapterEventDTOChapterUuid <$>
    upgrade _editChapterEventDTOTitle <*>
    upgrade _editChapterEventDTOText <*>
    upgrade _editChapterEventDTOQuestionUuids

instance Upgradeable V3.DeleteChapterEventDTO V4.DeleteChapterEventDTO where
  upgrade (V3.DeleteChapterEventDTO {..}) =
    return $
    V4.DeleteChapterEventDTO
      _deleteChapterEventDTOUuid
      (getParentUuid _deleteChapterEventDTOPath)
      _deleteChapterEventDTOChapterUuid

instance Upgradeable V3.AddExpertEventDTO V4.AddExpertEventDTO where
  upgrade (V3.AddExpertEventDTO {..}) =
    return $
    V4.AddExpertEventDTO
      _addExpertEventDTOUuid
      (getParentUuid _addExpertEventDTOPath)
      _addExpertEventDTOExpertUuid
      _addExpertEventDTOName
      _addExpertEventDTOEmail

instance Upgradeable V3.EditExpertEventDTO V4.EditExpertEventDTO where
  upgrade (V3.EditExpertEventDTO {..}) =
    V4.EditExpertEventDTO _editExpertEventDTOUuid (getParentUuid _editExpertEventDTOPath) _editExpertEventDTOExpertUuid <$>
    upgrade _editExpertEventDTOName <*>
    upgrade _editExpertEventDTOEmail

instance Upgradeable V3.DeleteExpertEventDTO V4.DeleteExpertEventDTO where
  upgrade (V3.DeleteExpertEventDTO {..}) =
    return $
    V4.DeleteExpertEventDTO
      _deleteExpertEventDTOUuid
      (getParentUuid _deleteExpertEventDTOPath)
      _deleteExpertEventDTOExpertUuid

instance Upgradeable V3.AddIntegrationEventDTO V4.AddIntegrationEventDTO where
  upgrade (V3.AddIntegrationEventDTO {..}) =
    return $
    V4.AddIntegrationEventDTO
      _addIntegrationEventDTOUuid
      (getParentUuid _addIntegrationEventDTOPath)
      _addIntegrationEventDTOIntegrationUuid
      _addIntegrationEventDTOIId
      _addIntegrationEventDTOName
      _addIntegrationEventDTOProps
      _addIntegrationEventDTOLogo
      _addIntegrationEventDTORequestMethod
      _addIntegrationEventDTORequestUrl
      _addIntegrationEventDTORequestHeaders
      _addIntegrationEventDTORequestBody
      _addIntegrationEventDTOResponseListField
      _addIntegrationEventDTOResponseIdField
      _addIntegrationEventDTOResponseNameField
      _addIntegrationEventDTOItemUrl

instance Upgradeable V3.EditIntegrationEventDTO V4.EditIntegrationEventDTO where
  upgrade (V3.EditIntegrationEventDTO {..}) =
    V4.EditIntegrationEventDTO
      _editIntegrationEventDTOUuid
      (getParentUuid _editIntegrationEventDTOPath)
      _editIntegrationEventDTOIntegrationUuid <$>
    upgrade _editIntegrationEventDTOIId <*>
    upgrade _editIntegrationEventDTOName <*>
    upgrade _editIntegrationEventDTOProps <*>
    upgrade _editIntegrationEventDTOLogo <*>
    upgrade _editIntegrationEventDTORequestMethod <*>
    upgrade _editIntegrationEventDTORequestUrl <*>
    upgrade _editIntegrationEventDTORequestHeaders <*>
    upgrade _editIntegrationEventDTORequestBody <*>
    upgrade _editIntegrationEventDTOResponseListField <*>
    upgrade _editIntegrationEventDTOResponseIdField <*>
    upgrade _editIntegrationEventDTOResponseNameField <*>
    upgrade _editIntegrationEventDTOItemUrl

instance Upgradeable V3.DeleteIntegrationEventDTO V4.DeleteIntegrationEventDTO where
  upgrade (V3.DeleteIntegrationEventDTO {..}) =
    return $
    V4.DeleteIntegrationEventDTO
      _deleteIntegrationEventDTOUuid
      (getParentUuid _deleteIntegrationEventDTOPath)
      _deleteIntegrationEventDTOIntegrationUuid

instance Upgradeable V3.AddKnowledgeModelEventDTO V4.AddKnowledgeModelEventDTO where
  upgrade (V3.AddKnowledgeModelEventDTO {..}) =
    return $
    V4.AddKnowledgeModelEventDTO
      _addKnowledgeModelEventDTOUuid
      U.nil
      _addKnowledgeModelEventDTOKmUuid
      _addKnowledgeModelEventDTOName

instance Upgradeable V3.EditKnowledgeModelEventDTO V4.EditKnowledgeModelEventDTO where
  upgrade (V3.EditKnowledgeModelEventDTO {..}) =
    V4.EditKnowledgeModelEventDTO
      _editKnowledgeModelEventDTOUuid
      U.nil
      _editKnowledgeModelEventDTOKmUuid <$>
      upgrade _editKnowledgeModelEventDTOName <*>
      upgrade _editKnowledgeModelEventDTOChapterUuids <*>
      upgrade _editKnowledgeModelEventDTOTagUuids <*>
      upgrade _editKnowledgeModelEventDTOIntegrationUuids

instance Upgradeable V3.AddQuestionEventDTO V4.AddQuestionEventDTO where
  upgrade (V3.AddOptionsQuestionEventDTO' q) = V4.AddOptionsQuestionEventDTO' <$> upgrade q
  upgrade (V3.AddListQuestionEventDTO' q) = V4.AddListQuestionEventDTO' <$> upgrade q
  upgrade (V3.AddValueQuestionEventDTO' q) = V4.AddValueQuestionEventDTO' <$> upgrade q
  upgrade (V3.AddIntegrationQuestionEventDTO' q) = V4.AddIntegrationQuestionEventDTO' <$> upgrade q

instance Upgradeable V3.AddOptionsQuestionEventDTO V4.AddOptionsQuestionEventDTO where
  upgrade (V3.AddOptionsQuestionEventDTO {..}) =
    return $
    V4.AddOptionsQuestionEventDTO
      _addOptionsQuestionEventDTOUuid
      (getParentUuid _addOptionsQuestionEventDTOPath)
      _addOptionsQuestionEventDTOQuestionUuid
      _addOptionsQuestionEventDTOTitle
      _addOptionsQuestionEventDTOText
      _addOptionsQuestionEventDTORequiredLevel
      _addOptionsQuestionEventDTOTagUuids

instance Upgradeable V3.AddListQuestionEventDTO V4.AddListQuestionEventDTO where
  upgrade (V3.AddListQuestionEventDTO {..}) =
    return $
    V4.AddListQuestionEventDTO
      _addListQuestionEventDTOUuid
      (getParentUuid _addListQuestionEventDTOPath)
      _addListQuestionEventDTOQuestionUuid
      _addListQuestionEventDTOTitle
      _addListQuestionEventDTOText
      _addListQuestionEventDTORequiredLevel
      _addListQuestionEventDTOTagUuids
      _addListQuestionEventDTOItemTemplateTitle

instance Upgradeable V3.AddValueQuestionEventDTO V4.AddValueQuestionEventDTO where
  upgrade (V3.AddValueQuestionEventDTO {..}) =
    V4.AddValueQuestionEventDTO
      _addValueQuestionEventDTOUuid
      (getParentUuid _addValueQuestionEventDTOPath)
      _addValueQuestionEventDTOQuestionUuid
      _addValueQuestionEventDTOTitle
      _addValueQuestionEventDTOText
      _addValueQuestionEventDTORequiredLevel
      _addValueQuestionEventDTOTagUuids <$>
    upgrade _addValueQuestionEventDTOValueType

instance Upgradeable V3.AddIntegrationQuestionEventDTO V4.AddIntegrationQuestionEventDTO where
  upgrade (V3.AddIntegrationQuestionEventDTO {..}) =
    return $
    V4.AddIntegrationQuestionEventDTO
      _addIntegrationQuestionEventDTOUuid
      (getParentUuid _addIntegrationQuestionEventDTOPath)
      _addIntegrationQuestionEventDTOQuestionUuid
      _addIntegrationQuestionEventDTOTitle
      _addIntegrationQuestionEventDTOText
      _addIntegrationQuestionEventDTORequiredLevel
      _addIntegrationQuestionEventDTOTagUuids
      _addIntegrationQuestionEventDTOIntegrationUuid
      _addIntegrationQuestionEventDTOProps

instance Upgradeable V3.EditQuestionEventDTO V4.EditQuestionEventDTO where
  upgrade (V3.EditOptionsQuestionEventDTO' q) = V4.EditOptionsQuestionEventDTO' <$> upgrade q
  upgrade (V3.EditListQuestionEventDTO' q) = V4.EditListQuestionEventDTO' <$> upgrade q
  upgrade (V3.EditValueQuestionEventDTO' q) = V4.EditValueQuestionEventDTO' <$> upgrade q
  upgrade (V3.EditIntegrationQuestionEventDTO' q) = V4.EditIntegrationQuestionEventDTO' <$> upgrade q

instance Upgradeable V3.EditOptionsQuestionEventDTO V4.EditOptionsQuestionEventDTO where
  upgrade (V3.EditOptionsQuestionEventDTO {..}) =
    V4.EditOptionsQuestionEventDTO
      _editOptionsQuestionEventDTOUuid
      (getParentUuid _editOptionsQuestionEventDTOPath)
      _editOptionsQuestionEventDTOQuestionUuid <$>
    upgrade _editOptionsQuestionEventDTOTitle <*>
    upgrade _editOptionsQuestionEventDTOText <*>
    upgrade _editOptionsQuestionEventDTORequiredLevel <*>
    upgrade _editOptionsQuestionEventDTOTagUuids <*>
    upgrade _editOptionsQuestionEventDTOExpertUuids <*>
    upgrade _editOptionsQuestionEventDTOReferenceUuids <*>
    upgrade _editOptionsQuestionEventDTOAnswerUuids

instance Upgradeable V3.EditListQuestionEventDTO V4.EditListQuestionEventDTO where
  upgrade (V3.EditListQuestionEventDTO {..}) =
    V4.EditListQuestionEventDTO
      _editListQuestionEventDTOUuid
      (getParentUuid _editListQuestionEventDTOPath)
      _editListQuestionEventDTOQuestionUuid <$>
    upgrade _editListQuestionEventDTOTitle <*>
    upgrade _editListQuestionEventDTOText <*>
    upgrade _editListQuestionEventDTORequiredLevel <*>
    upgrade _editListQuestionEventDTOTagUuids <*>
    upgrade _editListQuestionEventDTOExpertUuids <*>
    upgrade _editListQuestionEventDTOReferenceUuids <*>
    upgrade _editListQuestionEventDTOItemTemplateTitle <*>
    upgrade _editListQuestionEventDTOItemTemplateQuestionUuids

instance Upgradeable V3.EditValueQuestionEventDTO V4.EditValueQuestionEventDTO where
  upgrade (V3.EditValueQuestionEventDTO {..}) =
    V4.EditValueQuestionEventDTO
      _editValueQuestionEventDTOUuid
      (getParentUuid _editValueQuestionEventDTOPath)
      _editValueQuestionEventDTOQuestionUuid <$>
    upgrade _editValueQuestionEventDTOTitle <*>
    upgrade _editValueQuestionEventDTOText <*>
    upgrade _editValueQuestionEventDTORequiredLevel <*>
    upgrade _editValueQuestionEventDTOTagUuids <*>
    upgrade _editValueQuestionEventDTOExpertUuids <*>
    upgrade _editValueQuestionEventDTOReferenceUuids <*>
    upgrade _editValueQuestionEventDTOValueType

instance Upgradeable V3.EditIntegrationQuestionEventDTO V4.EditIntegrationQuestionEventDTO where
  upgrade (V3.EditIntegrationQuestionEventDTO {..}) =
    V4.EditIntegrationQuestionEventDTO
      _editIntegrationQuestionEventDTOUuid
      (getParentUuid _editIntegrationQuestionEventDTOPath)
      _editIntegrationQuestionEventDTOQuestionUuid <$>
    upgrade _editIntegrationQuestionEventDTOTitle <*>
    upgrade _editIntegrationQuestionEventDTOText <*>
    upgrade _editIntegrationQuestionEventDTORequiredLevel <*>
    upgrade _editIntegrationQuestionEventDTOTagUuids <*>
    upgrade _editIntegrationQuestionEventDTOExpertUuids <*>
    upgrade _editIntegrationQuestionEventDTOReferenceUuids <*>
    upgrade _editIntegrationQuestionEventDTOIntegrationUuid <*>
    upgrade _editIntegrationQuestionEventDTOProps

instance Upgradeable V3.DeleteQuestionEventDTO V4.DeleteQuestionEventDTO where
  upgrade (V3.DeleteQuestionEventDTO {..}) =
    return $
    V4.DeleteQuestionEventDTO
      _deleteQuestionEventDTOUuid
      (getParentUuid _deleteQuestionEventDTOPath)
      _deleteQuestionEventDTOQuestionUuid

instance Upgradeable V3.AddReferenceEventDTO V4.AddReferenceEventDTO where
  upgrade (V3.AddResourcePageReferenceEventDTO' r) = V4.AddResourcePageReferenceEventDTO' <$> upgrade r
  upgrade (V3.AddURLReferenceEventDTO' r) = V4.AddURLReferenceEventDTO' <$> upgrade r
  upgrade (V3.AddCrossReferenceEventDTO' r) = V4.AddCrossReferenceEventDTO' <$> upgrade r

instance Upgradeable V3.AddResourcePageReferenceEventDTO V4.AddResourcePageReferenceEventDTO where
  upgrade (V3.AddResourcePageReferenceEventDTO {..}) =
    return $
    V4.AddResourcePageReferenceEventDTO
      _addResourcePageReferenceEventDTOUuid
      (getParentUuid _addResourcePageReferenceEventDTOPath)
      _addResourcePageReferenceEventDTOReferenceUuid
      _addResourcePageReferenceEventDTOShortUuid

instance Upgradeable V3.AddURLReferenceEventDTO V4.AddURLReferenceEventDTO where
  upgrade (V3.AddURLReferenceEventDTO {..}) =
    return $
    V4.AddURLReferenceEventDTO
      _addURLReferenceEventDTOUuid
      (getParentUuid _addURLReferenceEventDTOPath)
      _addURLReferenceEventDTOReferenceUuid
      _addURLReferenceEventDTOUrl
      _addURLReferenceEventDTOLabel

instance Upgradeable V3.AddCrossReferenceEventDTO V4.AddCrossReferenceEventDTO where
  upgrade (V3.AddCrossReferenceEventDTO {..}) =
    return $
    V4.AddCrossReferenceEventDTO
      _addCrossReferenceEventDTOUuid
      (getParentUuid _addCrossReferenceEventDTOPath)
      _addCrossReferenceEventDTOReferenceUuid
      _addCrossReferenceEventDTOTargetUuid
      _addCrossReferenceEventDTODescription

instance Upgradeable V3.EditReferenceEventDTO V4.EditReferenceEventDTO where
  upgrade (V3.EditResourcePageReferenceEventDTO' r) = V4.EditResourcePageReferenceEventDTO' <$> upgrade r
  upgrade (V3.EditURLReferenceEventDTO' r) = V4.EditURLReferenceEventDTO' <$> upgrade r
  upgrade (V3.EditCrossReferenceEventDTO' r) = V4.EditCrossReferenceEventDTO' <$> upgrade r

instance Upgradeable V3.EditResourcePageReferenceEventDTO V4.EditResourcePageReferenceEventDTO where
  upgrade (V3.EditResourcePageReferenceEventDTO {..}) =
    V4.EditResourcePageReferenceEventDTO
      _editResourcePageReferenceEventDTOUuid
      (getParentUuid _editResourcePageReferenceEventDTOPath)
      _editResourcePageReferenceEventDTOReferenceUuid <$>
    upgrade _editResourcePageReferenceEventDTOShortUuid

instance Upgradeable V3.EditURLReferenceEventDTO V4.EditURLReferenceEventDTO where
  upgrade (V3.EditURLReferenceEventDTO {..}) =
    V4.EditURLReferenceEventDTO
      _editURLReferenceEventDTOUuid
      (getParentUuid _editURLReferenceEventDTOPath)
      _editURLReferenceEventDTOReferenceUuid <$>
    upgrade _editURLReferenceEventDTOUrl <*>
    upgrade _editURLReferenceEventDTOLabel

instance Upgradeable V3.EditCrossReferenceEventDTO V4.EditCrossReferenceEventDTO where
  upgrade (V3.EditCrossReferenceEventDTO {..}) =
    V4.EditCrossReferenceEventDTO
      _editCrossReferenceEventDTOUuid
      (getParentUuid _editCrossReferenceEventDTOPath)
      _editCrossReferenceEventDTOReferenceUuid <$>
    upgrade _editCrossReferenceEventDTOTargetUuid <*>
    upgrade _editCrossReferenceEventDTODescription

instance Upgradeable V3.DeleteReferenceEventDTO V4.DeleteReferenceEventDTO where
  upgrade (V3.DeleteReferenceEventDTO {..}) =
    return $
    V4.DeleteReferenceEventDTO
      _deleteReferenceEventDTOUuid
      (getParentUuid _deleteReferenceEventDTOPath)
      _deleteReferenceEventDTOReferenceUuid

instance Upgradeable V3.AddTagEventDTO V4.AddTagEventDTO where
  upgrade (V3.AddTagEventDTO {..}) =
    return $
    V4.AddTagEventDTO
      _addTagEventDTOUuid
      (getParentUuid _addTagEventDTOPath)
      _addTagEventDTOTagUuid
      _addTagEventDTOName
      _addTagEventDTODescription
      _addTagEventDTOColor

instance Upgradeable V3.EditTagEventDTO V4.EditTagEventDTO where
  upgrade (V3.EditTagEventDTO {..}) =
    V4.EditTagEventDTO _editTagEventDTOUuid (getParentUuid _editTagEventDTOPath) _editTagEventDTOTagUuid <$>
    upgrade _editTagEventDTOName <*>
    upgrade _editTagEventDTODescription <*>
    upgrade _editTagEventDTOColor

instance Upgradeable V3.DeleteTagEventDTO V4.DeleteTagEventDTO where
  upgrade (V3.DeleteTagEventDTO {..}) =
    return $
    V4.DeleteTagEventDTO _deleteTagEventDTOUuid (getParentUuid _deleteTagEventDTOPath) _deleteTagEventDTOTagUuid

instance Upgradeable V3.EventDTO V4.EventDTO where
  upgrade (V3.AddKnowledgeModelEventDTO' event) = V4.AddKnowledgeModelEventDTO' <$> upgrade event
  upgrade (V3.EditKnowledgeModelEventDTO' event) = V4.EditKnowledgeModelEventDTO' <$> upgrade event
  upgrade (V3.AddChapterEventDTO' event) = V4.AddChapterEventDTO' <$> upgrade event
  upgrade (V3.EditChapterEventDTO' event) = V4.EditChapterEventDTO' <$> upgrade event
  upgrade (V3.DeleteChapterEventDTO' event) = V4.DeleteChapterEventDTO' <$> upgrade event
  upgrade (V3.AddQuestionEventDTO' event) = V4.AddQuestionEventDTO' <$> upgrade event
  upgrade (V3.EditQuestionEventDTO' event) = V4.EditQuestionEventDTO' <$> upgrade event
  upgrade (V3.DeleteQuestionEventDTO' event) = V4.DeleteQuestionEventDTO' <$> upgrade event
  upgrade (V3.AddAnswerEventDTO' event) = V4.AddAnswerEventDTO' <$> upgrade event
  upgrade (V3.EditAnswerEventDTO' event) = V4.EditAnswerEventDTO' <$> upgrade event
  upgrade (V3.DeleteAnswerEventDTO' event) = V4.DeleteAnswerEventDTO' <$> upgrade event
  upgrade (V3.AddExpertEventDTO' event) = V4.AddExpertEventDTO' <$> upgrade event
  upgrade (V3.EditExpertEventDTO' event) = V4.EditExpertEventDTO' <$> upgrade event
  upgrade (V3.DeleteExpertEventDTO' event) = V4.DeleteExpertEventDTO' <$> upgrade event
  upgrade (V3.AddReferenceEventDTO' event) = V4.AddReferenceEventDTO' <$> upgrade event
  upgrade (V3.EditReferenceEventDTO' event) = V4.EditReferenceEventDTO' <$> upgrade event
  upgrade (V3.DeleteReferenceEventDTO' event) = V4.DeleteReferenceEventDTO' <$> upgrade event
  upgrade (V3.AddTagEventDTO' event) = V4.AddTagEventDTO' <$> upgrade event
  upgrade (V3.EditTagEventDTO' event) = V4.EditTagEventDTO' <$> upgrade event
  upgrade (V3.DeleteTagEventDTO' event) = V4.DeleteTagEventDTO' <$> upgrade event
  upgrade (V3.AddIntegrationEventDTO' event) = V4.AddIntegrationEventDTO' <$> upgrade event
  upgrade (V3.EditIntegrationEventDTO' event) = V4.EditIntegrationEventDTO' <$> upgrade event
  upgrade (V3.DeleteIntegrationEventDTO' event) = V4.DeleteIntegrationEventDTO' <$> upgrade event

migrateEventValue :: Value -> Either String Value
migrateEventValue input = do
  oldEvent <- result2Either (fromJSON input)
  newEvent <- upgrade (oldEvent :: V3.EventDTO)
  return $ toJSON (newEvent :: V4.EventDTO)
