module Wizard.Service.KnowledgeModel.Compiler.Modifier.Question where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U

import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Integration.IntegrationEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Integration.IntegrationEventLenses ()
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.KnowledgeModelEventField
import Shared.KnowledgeModel.Model.KnowledgeModel.Event.Question.QuestionEvent
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.KnowledgeModel.Compiler.Modifier.Modifier

instance CreateEntity AddQuestionEvent Question where
  createEntity event (AddOptionsQuestionEvent' content) =
    OptionsQuestion' $
      OptionsQuestion
        { uuid = event.entityUuid
        , title = content.title
        , text = content.text
        , requiredPhaseUuid = content.requiredPhaseUuid
        , annotations = content.annotations
        , tagUuids = content.tagUuids
        , referenceUuids = []
        , expertUuids = []
        , answerUuids = []
        }
  createEntity event (AddMultiChoiceQuestionEvent' content) =
    MultiChoiceQuestion' $
      MultiChoiceQuestion
        { uuid = event.entityUuid
        , title = content.title
        , text = content.text
        , requiredPhaseUuid = content.requiredPhaseUuid
        , annotations = content.annotations
        , tagUuids = content.tagUuids
        , referenceUuids = []
        , expertUuids = []
        , choiceUuids = []
        }
  createEntity event (AddListQuestionEvent' content) =
    ListQuestion' $
      ListQuestion
        { uuid = event.entityUuid
        , title = content.title
        , text = content.text
        , requiredPhaseUuid = content.requiredPhaseUuid
        , annotations = content.annotations
        , tagUuids = content.tagUuids
        , referenceUuids = []
        , expertUuids = []
        , itemTemplateQuestionUuids = []
        }
  createEntity event (AddValueQuestionEvent' content) =
    ValueQuestion' $
      ValueQuestion
        { uuid = event.entityUuid
        , title = content.title
        , text = content.text
        , requiredPhaseUuid = content.requiredPhaseUuid
        , annotations = content.annotations
        , tagUuids = content.tagUuids
        , referenceUuids = []
        , expertUuids = []
        , valueType = content.valueType
        , validations = content.validations
        }
  createEntity event (AddIntegrationQuestionEvent' content) =
    IntegrationQuestion' $
      IntegrationQuestion
        { uuid = event.entityUuid
        , title = content.title
        , text = content.text
        , requiredPhaseUuid = content.requiredPhaseUuid
        , annotations = content.annotations
        , tagUuids = content.tagUuids
        , referenceUuids = []
        , expertUuids = []
        , integrationUuid = content.integrationUuid
        , variables = content.variables
        }
  createEntity event (AddItemSelectQuestionEvent' content) =
    ItemSelectQuestion' $
      ItemSelectQuestion
        { uuid = event.entityUuid
        , title = content.title
        , text = content.text
        , requiredPhaseUuid = content.requiredPhaseUuid
        , annotations = content.annotations
        , tagUuids = content.tagUuids
        , referenceUuids = []
        , expertUuids = []
        , listQuestionUuid = content.listQuestionUuid
        }
  createEntity event (AddFileQuestionEvent' content) =
    FileQuestion' $
      FileQuestion
        { uuid = event.entityUuid
        , title = content.title
        , text = content.text
        , requiredPhaseUuid = content.requiredPhaseUuid
        , annotations = content.annotations
        , tagUuids = content.tagUuids
        , referenceUuids = []
        , expertUuids = []
        , maxSize = content.maxSize
        , fileTypes = content.fileTypes
        }

instance EditEntity EditQuestionEvent Question where
  editEntity event content' q =
    case content' of
      (EditOptionsQuestionEvent' content) -> applyToOptionsQuestion content . convertToOptionsQuestion $ q
      (EditListQuestionEvent' content) -> applyToListQuestion content . convertToListQuestion $ q
      (EditMultiChoiceQuestionEvent' content) -> applyToMultiChoiceQuestion content . convertToMultiChoiceQuestion $ q
      (EditValueQuestionEvent' content) -> applyToValueQuestion content . convertToValueQuestion $ q
      (EditIntegrationQuestionEvent' content) -> applyToIntegrationQuestion content . convertToIntegrationQuestion $ q
      (EditItemSelectQuestionEvent' content) -> applyToItemSelectQuestion content . convertToItemSelectQuestion $ q
      (EditFileQuestionEvent' content) -> applyToFileQuestion content . convertToFileQuestion $ q
    where
      applyToOptionsQuestion content optionQuestion =
        OptionsQuestion' $
          optionQuestion
            { title = applyValue optionQuestion.title content.title
            , text = applyValue optionQuestion.text content.text
            , requiredPhaseUuid = applyValue optionQuestion.requiredPhaseUuid content.requiredPhaseUuid
            , annotations = applyValue optionQuestion.annotations content.annotations
            , tagUuids = applyValue optionQuestion.tagUuids content.tagUuids
            , referenceUuids = applyValue optionQuestion.referenceUuids content.referenceUuids
            , expertUuids = applyValue optionQuestion.expertUuids content.expertUuids
            , answerUuids = applyValue optionQuestion.answerUuids content.answerUuids
            }
      applyToListQuestion content listQuestion =
        ListQuestion' $
          listQuestion
            { title = applyValue listQuestion.title content.title
            , text = applyValue listQuestion.text content.text
            , requiredPhaseUuid = applyValue listQuestion.requiredPhaseUuid content.requiredPhaseUuid
            , annotations = applyValue listQuestion.annotations content.annotations
            , tagUuids = applyValue listQuestion.tagUuids content.tagUuids
            , referenceUuids = applyValue listQuestion.referenceUuids content.referenceUuids
            , expertUuids = applyValue listQuestion.expertUuids content.expertUuids
            , itemTemplateQuestionUuids = applyValue listQuestion.itemTemplateQuestionUuids content.itemTemplateQuestionUuids
            }
      applyToMultiChoiceQuestion content multiChoiceQuestion =
        MultiChoiceQuestion' $
          multiChoiceQuestion
            { title = applyValue multiChoiceQuestion.title content.title
            , text = applyValue multiChoiceQuestion.text content.text
            , requiredPhaseUuid = applyValue multiChoiceQuestion.requiredPhaseUuid content.requiredPhaseUuid
            , annotations = applyValue multiChoiceQuestion.annotations content.annotations
            , tagUuids = applyValue multiChoiceQuestion.tagUuids content.tagUuids
            , referenceUuids = applyValue multiChoiceQuestion.referenceUuids content.referenceUuids
            , expertUuids = applyValue multiChoiceQuestion.expertUuids content.expertUuids
            , choiceUuids = applyValue multiChoiceQuestion.choiceUuids content.choiceUuids
            }
      applyToValueQuestion content valueQuestion =
        ValueQuestion' $
          valueQuestion
            { title = applyValue valueQuestion.title content.title
            , text = applyValue valueQuestion.text content.text
            , requiredPhaseUuid = applyValue valueQuestion.requiredPhaseUuid content.requiredPhaseUuid
            , annotations = applyValue valueQuestion.annotations content.annotations
            , tagUuids = applyValue valueQuestion.tagUuids content.tagUuids
            , referenceUuids = applyValue valueQuestion.referenceUuids content.referenceUuids
            , expertUuids = applyValue valueQuestion.expertUuids content.expertUuids
            , valueType = applyValue valueQuestion.valueType content.valueType
            , validations = applyValue valueQuestion.validations content.validations
            }
      applyToIntegrationQuestion content integrationQuestion =
        IntegrationQuestion' $
          integrationQuestion
            { title = applyValue integrationQuestion.title content.title
            , text = applyValue integrationQuestion.text content.text
            , requiredPhaseUuid = applyValue integrationQuestion.requiredPhaseUuid content.requiredPhaseUuid
            , annotations = applyValue integrationQuestion.annotations content.annotations
            , tagUuids = applyValue integrationQuestion.tagUuids content.tagUuids
            , referenceUuids = applyValue integrationQuestion.referenceUuids content.referenceUuids
            , expertUuids = applyValue integrationQuestion.expertUuids content.expertUuids
            , integrationUuid = applyValue integrationQuestion.integrationUuid content.integrationUuid
            , variables = applyValue integrationQuestion.variables content.variables
            }
      applyToItemSelectQuestion content itemSelectQuestion =
        ItemSelectQuestion' $
          itemSelectQuestion
            { title = applyValue itemSelectQuestion.title content.title
            , text = applyValue itemSelectQuestion.text content.text
            , requiredPhaseUuid = applyValue itemSelectQuestion.requiredPhaseUuid content.requiredPhaseUuid
            , annotations = applyValue itemSelectQuestion.annotations content.annotations
            , tagUuids = applyValue itemSelectQuestion.tagUuids content.tagUuids
            , referenceUuids = applyValue itemSelectQuestion.referenceUuids content.referenceUuids
            , expertUuids = applyValue itemSelectQuestion.expertUuids content.expertUuids
            , listQuestionUuid = applyValue itemSelectQuestion.listQuestionUuid content.listQuestionUuid
            }
      applyToFileQuestion content fileQuestion =
        FileQuestion' $
          fileQuestion
            { title = applyValue fileQuestion.title content.title
            , text = applyValue fileQuestion.text content.text
            , requiredPhaseUuid = applyValue fileQuestion.requiredPhaseUuid content.requiredPhaseUuid
            , annotations = applyValue fileQuestion.annotations content.annotations
            , tagUuids = applyValue fileQuestion.tagUuids content.tagUuids
            , referenceUuids = applyValue fileQuestion.referenceUuids content.referenceUuids
            , expertUuids = applyValue fileQuestion.expertUuids content.expertUuids
            , maxSize = applyValue fileQuestion.maxSize content.maxSize
            , fileTypes = applyValue fileQuestion.fileTypes content.fileTypes
            }

convertToOptionsQuestion :: Question -> OptionsQuestion
convertToOptionsQuestion q' =
  case q' of
    (OptionsQuestion' q) -> q
    (MultiChoiceQuestion' q) -> createQuestion q
    (ListQuestion' q) -> createQuestion q
    (ValueQuestion' q) -> createQuestion q
    (IntegrationQuestion' q) -> createQuestion q
    (ItemSelectQuestion' q) -> createQuestion q
    (FileQuestion' q) -> createQuestion q
  where
    createQuestion q =
      OptionsQuestion
        { uuid = q.uuid
        , title = q.title
        , text = q.text
        , requiredPhaseUuid = q.requiredPhaseUuid
        , annotations = q.annotations
        , tagUuids = q.tagUuids
        , referenceUuids = q.referenceUuids
        , expertUuids = q.expertUuids
        , answerUuids = []
        }

convertToListQuestion :: Question -> ListQuestion
convertToListQuestion q' =
  case q' of
    (OptionsQuestion' q) -> createQuestion q
    (MultiChoiceQuestion' q) -> createQuestion q
    (ListQuestion' q) -> q
    (ValueQuestion' q) -> createQuestion q
    (IntegrationQuestion' q) -> createQuestion q
    (ItemSelectQuestion' q) -> createQuestion q
    (FileQuestion' q) -> createQuestion q
  where
    createQuestion q =
      ListQuestion
        { uuid = q.uuid
        , title = q.title
        , text = q.text
        , requiredPhaseUuid = q.requiredPhaseUuid
        , annotations = q.annotations
        , tagUuids = q.tagUuids
        , referenceUuids = q.referenceUuids
        , expertUuids = q.expertUuids
        , itemTemplateQuestionUuids = []
        }

convertToMultiChoiceQuestion :: Question -> MultiChoiceQuestion
convertToMultiChoiceQuestion q' =
  case q' of
    (OptionsQuestion' q) -> createQuestion q
    (MultiChoiceQuestion' q) -> q
    (ListQuestion' q) -> createQuestion q
    (ValueQuestion' q) -> createQuestion q
    (IntegrationQuestion' q) -> createQuestion q
    (ItemSelectQuestion' q) -> createQuestion q
    (FileQuestion' q) -> createQuestion q
  where
    createQuestion q =
      MultiChoiceQuestion
        { uuid = q.uuid
        , title = q.title
        , text = q.text
        , requiredPhaseUuid = q.requiredPhaseUuid
        , annotations = q.annotations
        , tagUuids = q.tagUuids
        , referenceUuids = q.referenceUuids
        , expertUuids = q.expertUuids
        , choiceUuids = []
        }

convertToValueQuestion :: Question -> ValueQuestion
convertToValueQuestion q' =
  case q' of
    (OptionsQuestion' q) -> createQuestion q
    (MultiChoiceQuestion' q) -> createQuestion q
    (ListQuestion' q) -> createQuestion q
    (ValueQuestion' q) -> q
    (IntegrationQuestion' q) -> createQuestion q
    (ItemSelectQuestion' q) -> createQuestion q
    (FileQuestion' q) -> createQuestion q
  where
    createQuestion q =
      ValueQuestion
        { uuid = q.uuid
        , title = q.title
        , text = q.text
        , requiredPhaseUuid = q.requiredPhaseUuid
        , annotations = q.annotations
        , tagUuids = q.tagUuids
        , referenceUuids = q.referenceUuids
        , expertUuids = q.expertUuids
        , valueType = StringQuestionValueType
        , validations = []
        }

convertToIntegrationQuestion :: Question -> IntegrationQuestion
convertToIntegrationQuestion q' =
  case q' of
    (OptionsQuestion' q) -> createQuestion q
    (MultiChoiceQuestion' q) -> createQuestion q
    (ListQuestion' q) -> createQuestion q
    (ValueQuestion' q) -> createQuestion q
    (IntegrationQuestion' q) -> q
    (ItemSelectQuestion' q) -> createQuestion q
    (FileQuestion' q) -> createQuestion q
  where
    createQuestion q =
      IntegrationQuestion
        { uuid = q.uuid
        , title = q.title
        , text = q.text
        , requiredPhaseUuid = q.requiredPhaseUuid
        , annotations = q.annotations
        , tagUuids = q.tagUuids
        , referenceUuids = q.referenceUuids
        , expertUuids = q.expertUuids
        , integrationUuid = U.nil
        , variables = M.empty
        }

convertToItemSelectQuestion :: Question -> ItemSelectQuestion
convertToItemSelectQuestion q' =
  case q' of
    (OptionsQuestion' q) -> createQuestion q
    (MultiChoiceQuestion' q) -> createQuestion q
    (ListQuestion' q) -> createQuestion q
    (ValueQuestion' q) -> createQuestion q
    (IntegrationQuestion' q) -> createQuestion q
    (ItemSelectQuestion' q) -> q
    (FileQuestion' q) -> createQuestion q
  where
    createQuestion q =
      ItemSelectQuestion
        { uuid = q.uuid
        , title = q.title
        , text = q.text
        , requiredPhaseUuid = q.requiredPhaseUuid
        , annotations = q.annotations
        , tagUuids = q.tagUuids
        , referenceUuids = q.referenceUuids
        , expertUuids = q.expertUuids
        , listQuestionUuid = Nothing
        }

convertToFileQuestion :: Question -> FileQuestion
convertToFileQuestion q' =
  case q' of
    (OptionsQuestion' q) -> createQuestion q
    (MultiChoiceQuestion' q) -> createQuestion q
    (ListQuestion' q) -> createQuestion q
    (ValueQuestion' q) -> createQuestion q
    (IntegrationQuestion' q) -> createQuestion q
    (ItemSelectQuestion' q) -> createQuestion q
    (FileQuestion' q) -> q
  where
    createQuestion q =
      FileQuestion
        { uuid = q.uuid
        , title = q.title
        , text = q.text
        , requiredPhaseUuid = q.requiredPhaseUuid
        , annotations = q.annotations
        , tagUuids = q.tagUuids
        , referenceUuids = q.referenceUuids
        , expertUuids = q.expertUuids
        , maxSize = Nothing
        , fileTypes = Nothing
        }

updateIntegrationVariables :: KnowledgeModelEvent -> EditIntegrationEvent -> Question -> Question
updateIntegrationVariables event content (IntegrationQuestion' q) = IntegrationQuestion' $ q {variables = updatedVariables}
  where
    updatedVariables =
      if q.integrationUuid == event.entityUuid
        then case getVariables content of
          ChangedValue ps -> M.fromList . fmap (\p -> (p, fromMaybe "" (M.lookup p q.variables))) $ ps
          NothingChanged -> q.variables
        else q.variables
updateIntegrationVariables _ _ q' = q'

deleteIntegrationReference :: KnowledgeModelEvent -> Question -> Question
deleteIntegrationReference event (IntegrationQuestion' q) =
  if q.integrationUuid == event.entityUuid
    then ValueQuestion' $ convertToValueQuestion . IntegrationQuestion' $ q
    else IntegrationQuestion' q
deleteIntegrationReference _ q' = q'

deletePhaseReference :: KnowledgeModelEvent -> Question -> Question
deletePhaseReference event q' =
  case getRequiredPhaseUuid q' of
    Just requiredPhaseUuid ->
      if requiredPhaseUuid == event.entityUuid
        then setRequiredPhaseUuid q' Nothing
        else q'
    Nothing -> q'

deleteTagReference :: KnowledgeModelEvent -> Question -> Question
deleteTagReference event q = setTagUuids q (L.delete event.entityUuid (getTagUuids q))
