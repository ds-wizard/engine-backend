module Wizard.Service.KnowledgeModel.Compilator.Modifier.Question where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U

import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier
import WizardLib.KnowledgeModel.Model.Event.EventField
import WizardLib.KnowledgeModel.Model.Event.Integration.IntegrationEvent
import WizardLib.KnowledgeModel.Model.Event.Integration.IntegrationEventLenses ()
import WizardLib.KnowledgeModel.Model.Event.Phase.PhaseEvent
import WizardLib.KnowledgeModel.Model.Event.Question.QuestionEvent
import WizardLib.KnowledgeModel.Model.Event.Tag.TagEvent
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import WizardLib.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses

instance CreateEntity AddQuestionEvent Question where
  createEntity (AddOptionsQuestionEvent' event) =
    OptionsQuestion' $
      OptionsQuestion
        { uuid = event.entityUuid
        , title = event.title
        , text = event.text
        , requiredPhaseUuid = event.requiredPhaseUuid
        , annotations = event.annotations
        , tagUuids = event.tagUuids
        , referenceUuids = []
        , expertUuids = []
        , answerUuids = []
        }
  createEntity (AddMultiChoiceQuestionEvent' event) =
    MultiChoiceQuestion' $
      MultiChoiceQuestion
        { uuid = event.entityUuid
        , title = event.title
        , text = event.text
        , requiredPhaseUuid = event.requiredPhaseUuid
        , annotations = event.annotations
        , tagUuids = event.tagUuids
        , referenceUuids = []
        , expertUuids = []
        , choiceUuids = []
        }
  createEntity (AddListQuestionEvent' event) =
    ListQuestion' $
      ListQuestion
        { uuid = event.entityUuid
        , title = event.title
        , text = event.text
        , requiredPhaseUuid = event.requiredPhaseUuid
        , annotations = event.annotations
        , tagUuids = event.tagUuids
        , referenceUuids = []
        , expertUuids = []
        , itemTemplateQuestionUuids = []
        }
  createEntity (AddValueQuestionEvent' event) =
    ValueQuestion' $
      ValueQuestion
        { uuid = event.entityUuid
        , title = event.title
        , text = event.text
        , requiredPhaseUuid = event.requiredPhaseUuid
        , annotations = event.annotations
        , tagUuids = event.tagUuids
        , referenceUuids = []
        , expertUuids = []
        , valueType = event.valueType
        , validations = event.validations
        }
  createEntity (AddIntegrationQuestionEvent' event) =
    IntegrationQuestion' $
      IntegrationQuestion
        { uuid = event.entityUuid
        , title = event.title
        , text = event.text
        , requiredPhaseUuid = event.requiredPhaseUuid
        , annotations = event.annotations
        , tagUuids = event.tagUuids
        , referenceUuids = []
        , expertUuids = []
        , integrationUuid = event.integrationUuid
        , props = event.props
        }
  createEntity (AddItemSelectQuestionEvent' event) =
    ItemSelectQuestion' $
      ItemSelectQuestion
        { uuid = event.entityUuid
        , title = event.title
        , text = event.text
        , requiredPhaseUuid = event.requiredPhaseUuid
        , annotations = event.annotations
        , tagUuids = event.tagUuids
        , referenceUuids = []
        , expertUuids = []
        , listQuestionUuid = event.listQuestionUuid
        }
  createEntity (AddFileQuestionEvent' event) =
    FileQuestion' $
      FileQuestion
        { uuid = event.entityUuid
        , title = event.title
        , text = event.text
        , requiredPhaseUuid = event.requiredPhaseUuid
        , annotations = event.annotations
        , tagUuids = event.tagUuids
        , referenceUuids = []
        , expertUuids = []
        , maxSize = event.maxSize
        , fileTypes = event.fileTypes
        }

instance EditEntity EditQuestionEvent Question where
  editEntity event' q =
    case event' of
      (EditOptionsQuestionEvent' event) -> applyToOptionsQuestion event . convertToOptionsQuestion $ q
      (EditListQuestionEvent' event) -> applyToListQuestion event . convertToListQuestion $ q
      (EditMultiChoiceQuestionEvent' event) -> applyToMultiChoiceQuestion event . convertToMultiChoiceQuestion $ q
      (EditValueQuestionEvent' event) -> applyToValueQuestion event . convertToValueQuestion $ q
      (EditIntegrationQuestionEvent' event) -> applyToIntegrationQuestion event . convertToIntegrationQuestion $ q
      (EditItemSelectQuestionEvent' event) -> applyToItemSelectQuestion event . convertToItemSelectQuestion $ q
      (EditFileQuestionEvent' event) -> applyToFileQuestion event . convertToFileQuestion $ q
    where
      applyToOptionsQuestion event optionQuestion =
        OptionsQuestion' $
          optionQuestion
            { title = applyValue optionQuestion.title event.title
            , text = applyValue optionQuestion.text event.text
            , requiredPhaseUuid = applyValue optionQuestion.requiredPhaseUuid event.requiredPhaseUuid
            , annotations = applyValue optionQuestion.annotations event.annotations
            , tagUuids = applyValue optionQuestion.tagUuids event.tagUuids
            , referenceUuids = applyValue optionQuestion.referenceUuids event.referenceUuids
            , expertUuids = applyValue optionQuestion.expertUuids event.expertUuids
            , answerUuids = applyValue optionQuestion.answerUuids event.answerUuids
            }
      applyToListQuestion event listQuestion =
        ListQuestion' $
          listQuestion
            { title = applyValue listQuestion.title event.title
            , text = applyValue listQuestion.text event.text
            , requiredPhaseUuid = applyValue listQuestion.requiredPhaseUuid event.requiredPhaseUuid
            , annotations = applyValue listQuestion.annotations event.annotations
            , tagUuids = applyValue listQuestion.tagUuids event.tagUuids
            , referenceUuids = applyValue listQuestion.referenceUuids event.referenceUuids
            , expertUuids = applyValue listQuestion.expertUuids event.expertUuids
            , itemTemplateQuestionUuids = applyValue listQuestion.itemTemplateQuestionUuids event.itemTemplateQuestionUuids
            }
      applyToMultiChoiceQuestion event multiChoiceQuestion =
        MultiChoiceQuestion' $
          multiChoiceQuestion
            { title = applyValue multiChoiceQuestion.title event.title
            , text = applyValue multiChoiceQuestion.text event.text
            , requiredPhaseUuid = applyValue multiChoiceQuestion.requiredPhaseUuid event.requiredPhaseUuid
            , annotations = applyValue multiChoiceQuestion.annotations event.annotations
            , tagUuids = applyValue multiChoiceQuestion.tagUuids event.tagUuids
            , referenceUuids = applyValue multiChoiceQuestion.referenceUuids event.referenceUuids
            , expertUuids = applyValue multiChoiceQuestion.expertUuids event.expertUuids
            , choiceUuids = applyValue multiChoiceQuestion.choiceUuids event.choiceUuids
            }
      applyToValueQuestion event valueQuestion =
        ValueQuestion' $
          valueQuestion
            { title = applyValue valueQuestion.title event.title
            , text = applyValue valueQuestion.text event.text
            , requiredPhaseUuid = applyValue valueQuestion.requiredPhaseUuid event.requiredPhaseUuid
            , annotations = applyValue valueQuestion.annotations event.annotations
            , tagUuids = applyValue valueQuestion.tagUuids event.tagUuids
            , referenceUuids = applyValue valueQuestion.referenceUuids event.referenceUuids
            , expertUuids = applyValue valueQuestion.expertUuids event.expertUuids
            , valueType = applyValue valueQuestion.valueType event.valueType
            , validations = applyValue valueQuestion.validations event.validations
            }
      applyToIntegrationQuestion event integrationQuestion =
        IntegrationQuestion' $
          integrationQuestion
            { title = applyValue integrationQuestion.title event.title
            , text = applyValue integrationQuestion.text event.text
            , requiredPhaseUuid = applyValue integrationQuestion.requiredPhaseUuid event.requiredPhaseUuid
            , annotations = applyValue integrationQuestion.annotations event.annotations
            , tagUuids = applyValue integrationQuestion.tagUuids event.tagUuids
            , referenceUuids = applyValue integrationQuestion.referenceUuids event.referenceUuids
            , expertUuids = applyValue integrationQuestion.expertUuids event.expertUuids
            , integrationUuid = applyValue integrationQuestion.integrationUuid event.integrationUuid
            , props = applyValue integrationQuestion.props event.props
            }
      applyToItemSelectQuestion event itemSelectQuestion =
        ItemSelectQuestion' $
          itemSelectQuestion
            { title = applyValue itemSelectQuestion.title event.title
            , text = applyValue itemSelectQuestion.text event.text
            , requiredPhaseUuid = applyValue itemSelectQuestion.requiredPhaseUuid event.requiredPhaseUuid
            , annotations = applyValue itemSelectQuestion.annotations event.annotations
            , tagUuids = applyValue itemSelectQuestion.tagUuids event.tagUuids
            , referenceUuids = applyValue itemSelectQuestion.referenceUuids event.referenceUuids
            , expertUuids = applyValue itemSelectQuestion.expertUuids event.expertUuids
            , listQuestionUuid = applyValue itemSelectQuestion.listQuestionUuid event.listQuestionUuid
            }
      applyToFileQuestion event fileQuestion =
        FileQuestion' $
          fileQuestion
            { title = applyValue fileQuestion.title event.title
            , text = applyValue fileQuestion.text event.text
            , requiredPhaseUuid = applyValue fileQuestion.requiredPhaseUuid event.requiredPhaseUuid
            , annotations = applyValue fileQuestion.annotations event.annotations
            , tagUuids = applyValue fileQuestion.tagUuids event.tagUuids
            , referenceUuids = applyValue fileQuestion.referenceUuids event.referenceUuids
            , expertUuids = applyValue fileQuestion.expertUuids event.expertUuids
            , maxSize = applyValue fileQuestion.maxSize event.maxSize
            , fileTypes = applyValue fileQuestion.fileTypes event.fileTypes
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
        , props = M.empty
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

updateIntegrationProps :: EditIntegrationEvent -> Question -> Question
updateIntegrationProps event (IntegrationQuestion' q) = IntegrationQuestion' $ q {props = updatedProps}
  where
    updatedProps =
      if q.integrationUuid == getEntityUuid event
        then case getProps event of
          ChangedValue ps -> M.fromList . fmap (\p -> (p, fromMaybe "" (M.lookup p q.props))) $ ps
          NothingChanged -> q.props
        else q.props
updateIntegrationProps _ q' = q'

deleteIntegrationReference :: DeleteIntegrationEvent -> Question -> Question
deleteIntegrationReference event (IntegrationQuestion' q) =
  if q.integrationUuid == event.entityUuid
    then ValueQuestion' $ convertToValueQuestion . IntegrationQuestion' $ q
    else IntegrationQuestion' q
deleteIntegrationReference _ q' = q'

deletePhaseReference :: DeletePhaseEvent -> Question -> Question
deletePhaseReference event q' =
  case getRequiredPhaseUuid q' of
    Just requiredPhaseUuid ->
      if requiredPhaseUuid == event.entityUuid
        then setRequiredPhaseUuid q' Nothing
        else q'
    Nothing -> q'

deleteTagReference :: DeleteTagEvent -> Question -> Question
deleteTagReference event q = setTagUuids q (L.delete event.entityUuid (getTagUuids q))
