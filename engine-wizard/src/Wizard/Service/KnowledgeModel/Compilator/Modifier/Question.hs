module Wizard.Service.KnowledgeModel.Compilator.Modifier.Question where

import Control.Lens ((&), (.~), (^.))
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.UUID as U

import LensesConfig
import Shared.Model.Event.EventField
import Shared.Model.Event.Integration.IntegrationEvent
import Shared.Model.Event.Phase.PhaseEvent
import Shared.Model.Event.Question.QuestionEvent
import Shared.Model.Event.Tag.TagEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Shared.Model.KnowledgeModel.KnowledgeModelLenses
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddQuestionEvent Question where
  createEntity (AddOptionsQuestionEvent' e) =
    OptionsQuestion' $
    OptionsQuestion
      { _optionsQuestionUuid = e ^. entityUuid
      , _optionsQuestionTitle = e ^. title
      , _optionsQuestionText = e ^. text
      , _optionsQuestionRequiredPhaseUuid = e ^. requiredPhaseUuid
      , _optionsQuestionTagUuids = e ^. tagUuids
      , _optionsQuestionReferenceUuids = []
      , _optionsQuestionExpertUuids = []
      , _optionsQuestionAnswerUuids = []
      }
  createEntity (AddMultiChoiceQuestionEvent' e) =
    MultiChoiceQuestion' $
    MultiChoiceQuestion
      { _multiChoiceQuestionUuid = e ^. entityUuid
      , _multiChoiceQuestionTitle = e ^. title
      , _multiChoiceQuestionText = e ^. text
      , _multiChoiceQuestionRequiredPhaseUuid = e ^. requiredPhaseUuid
      , _multiChoiceQuestionTagUuids = e ^. tagUuids
      , _multiChoiceQuestionReferenceUuids = []
      , _multiChoiceQuestionExpertUuids = []
      , _multiChoiceQuestionChoiceUuids = []
      }
  createEntity (AddListQuestionEvent' e) =
    ListQuestion' $
    ListQuestion
      { _listQuestionUuid = e ^. entityUuid
      , _listQuestionTitle = e ^. title
      , _listQuestionText = e ^. text
      , _listQuestionRequiredPhaseUuid = e ^. requiredPhaseUuid
      , _listQuestionTagUuids = e ^. tagUuids
      , _listQuestionReferenceUuids = []
      , _listQuestionExpertUuids = []
      , _listQuestionItemTemplateQuestionUuids = []
      }
  createEntity (AddValueQuestionEvent' e) =
    ValueQuestion' $
    ValueQuestion
      { _valueQuestionUuid = e ^. entityUuid
      , _valueQuestionTitle = e ^. title
      , _valueQuestionText = e ^. text
      , _valueQuestionRequiredPhaseUuid = e ^. requiredPhaseUuid
      , _valueQuestionTagUuids = e ^. tagUuids
      , _valueQuestionReferenceUuids = []
      , _valueQuestionExpertUuids = []
      , _valueQuestionValueType = e ^. valueType
      }
  createEntity (AddIntegrationQuestionEvent' e) =
    IntegrationQuestion' $
    IntegrationQuestion
      { _integrationQuestionUuid = e ^. entityUuid
      , _integrationQuestionTitle = e ^. title
      , _integrationQuestionText = e ^. text
      , _integrationQuestionRequiredPhaseUuid = e ^. requiredPhaseUuid
      , _integrationQuestionTagUuids = e ^. tagUuids
      , _integrationQuestionReferenceUuids = []
      , _integrationQuestionExpertUuids = []
      , _integrationQuestionIntegrationUuid = e ^. integrationUuid
      , _integrationQuestionProps = e ^. props
      }

instance EditEntity EditQuestionEvent Question where
  editEntity e' q =
    case e' of
      (EditOptionsQuestionEvent' e) -> applyToOptionsQuestion e . convertToOptionsQuestion $ q
      (EditListQuestionEvent' e) -> applyToListQuestion e . convertToListQuestion $ q
      (EditMultiChoiceQuestionEvent' e) -> applyToMultiChoiceQuestion e . convertToMultiChoiceQuestion $ q
      (EditValueQuestionEvent' e) -> applyToValueQuestion e . convertToValueQuestion $ q
      (EditIntegrationQuestionEvent' e) -> applyToIntegrationQuestion e . convertToIntegrationQuestion $ q
    where
      applyToOptionsQuestion e =
        applyAnwerUuids e .
        applyReferenceUuids e . applyExpertUuids e . applyTagUuids e . applyRequiredLevel e . applyText e . applyTitle e
      applyToMultiChoiceQuestion e =
        applyChoiceUuids e .
        applyReferenceUuids e . applyExpertUuids e . applyTagUuids e . applyRequiredLevel e . applyText e . applyTitle e
      applyToListQuestion e =
        applyItemTemplateQuestionUuids e .
        applyReferenceUuids e . applyExpertUuids e . applyTagUuids e . applyRequiredLevel e . applyText e . applyTitle e
      applyToValueQuestion e =
        applyValueType e .
        applyReferenceUuids e . applyExpertUuids e . applyTagUuids e . applyRequiredLevel e . applyText e . applyTitle e
      applyToIntegrationQuestion e =
        applyProps e .
        applyIntegrationUuid e .
        applyReferenceUuids e . applyExpertUuids e . applyTagUuids e . applyRequiredLevel e . applyText e . applyTitle e
      applyTitle e q = applyValue (e ^. title) q title'
      applyText e q = applyValue (e ^. text) q text'
      applyRequiredLevel e q = applyValue (e ^. requiredPhaseUuid) q requiredPhaseUuid'
      applyTagUuids e q = applyValue (e ^. tagUuids) q tagUuids'
      applyExpertUuids e q = applyValue (e ^. expertUuids) q expertUuids'
      applyReferenceUuids e q = applyValue (e ^. referenceUuids) q referenceUuids'
      applyChoiceUuids e q = applyValue (e ^. choiceUuids) q choiceUuids'
      applyAnwerUuids e q = applyValue (e ^. answerUuids) q answerUuids'
      applyItemTemplateQuestionUuids e q = applyValue (e ^. itemTemplateQuestionUuids) q itemTemplateQuestionUuids'
      applyValueType e q = applyValue (e ^. valueType) q valueType'
      applyIntegrationUuid e q = applyValue (e ^. integrationUuid) q integrationUuid'
      applyProps e q = applyValue (e ^. props) q props'

convertToOptionsQuestion :: Question -> Question
convertToOptionsQuestion (OptionsQuestion' q) = OptionsQuestion' q
convertToOptionsQuestion q' =
  case q' of
    (MultiChoiceQuestion' q) -> createQuestion q
    (ListQuestion' q) -> createQuestion q
    (ValueQuestion' q) -> createQuestion q
    (IntegrationQuestion' q) -> createQuestion q
  where
    createQuestion q =
      OptionsQuestion' $
      OptionsQuestion
        { _optionsQuestionUuid = q ^. uuid
        , _optionsQuestionTitle = q ^. title
        , _optionsQuestionText = q ^. text
        , _optionsQuestionRequiredPhaseUuid = q ^. requiredPhaseUuid
        , _optionsQuestionTagUuids = q ^. tagUuids
        , _optionsQuestionReferenceUuids = q ^. referenceUuids
        , _optionsQuestionExpertUuids = q ^. expertUuids
        , _optionsQuestionAnswerUuids = []
        }

convertToMultiChoiceQuestion :: Question -> Question
convertToMultiChoiceQuestion (MultiChoiceQuestion' q) = MultiChoiceQuestion' q
convertToMultiChoiceQuestion q' =
  case q' of
    (OptionsQuestion' q) -> createQuestion q
    (ListQuestion' q) -> createQuestion q
    (ValueQuestion' q) -> createQuestion q
    (IntegrationQuestion' q) -> createQuestion q
  where
    createQuestion q =
      MultiChoiceQuestion' $
      MultiChoiceQuestion
        { _multiChoiceQuestionUuid = q ^. uuid
        , _multiChoiceQuestionTitle = q ^. title
        , _multiChoiceQuestionText = q ^. text
        , _multiChoiceQuestionRequiredPhaseUuid = q ^. requiredPhaseUuid
        , _multiChoiceQuestionTagUuids = q ^. tagUuids
        , _multiChoiceQuestionReferenceUuids = q ^. referenceUuids
        , _multiChoiceQuestionExpertUuids = q ^. expertUuids
        , _multiChoiceQuestionChoiceUuids = []
        }

convertToListQuestion :: Question -> Question
convertToListQuestion (ListQuestion' q) = ListQuestion' q
convertToListQuestion q' =
  case q' of
    (OptionsQuestion' q) -> createQuestion q
    (MultiChoiceQuestion' q) -> createQuestion q
    (ValueQuestion' q) -> createQuestion q
    (IntegrationQuestion' q) -> createQuestion q
  where
    createQuestion q =
      ListQuestion' $
      ListQuestion
        { _listQuestionUuid = q ^. uuid
        , _listQuestionTitle = q ^. title
        , _listQuestionText = q ^. text
        , _listQuestionRequiredPhaseUuid = q ^. requiredPhaseUuid
        , _listQuestionTagUuids = q ^. tagUuids
        , _listQuestionReferenceUuids = q ^. referenceUuids
        , _listQuestionExpertUuids = q ^. expertUuids
        , _listQuestionItemTemplateQuestionUuids = []
        }

convertToValueQuestion :: Question -> Question
convertToValueQuestion (ValueQuestion' q) = ValueQuestion' q
convertToValueQuestion q' =
  case q' of
    (OptionsQuestion' q) -> createQuestion q
    (MultiChoiceQuestion' q) -> createQuestion q
    (ListQuestion' q) -> createQuestion q
    (IntegrationQuestion' q) -> createQuestion q
  where
    createQuestion q =
      ValueQuestion' $
      ValueQuestion
        { _valueQuestionUuid = q ^. uuid
        , _valueQuestionTitle = q ^. title
        , _valueQuestionText = q ^. text
        , _valueQuestionRequiredPhaseUuid = q ^. requiredPhaseUuid
        , _valueQuestionTagUuids = q ^. tagUuids
        , _valueQuestionReferenceUuids = q ^. referenceUuids
        , _valueQuestionExpertUuids = q ^. expertUuids
        , _valueQuestionValueType = StringQuestionValueType
        }

convertToIntegrationQuestion :: Question -> Question
convertToIntegrationQuestion (IntegrationQuestion' q) = IntegrationQuestion' q
convertToIntegrationQuestion q' =
  case q' of
    (OptionsQuestion' q) -> createQuestion q
    (MultiChoiceQuestion' q) -> createQuestion q
    (ListQuestion' q) -> createQuestion q
    (ValueQuestion' q) -> createQuestion q
  where
    createQuestion q =
      IntegrationQuestion' $
      IntegrationQuestion
        { _integrationQuestionUuid = q ^. uuid
        , _integrationQuestionTitle = q ^. title
        , _integrationQuestionText = q ^. text
        , _integrationQuestionRequiredPhaseUuid = q ^. requiredPhaseUuid
        , _integrationQuestionTagUuids = q ^. tagUuids
        , _integrationQuestionReferenceUuids = q ^. referenceUuids
        , _integrationQuestionExpertUuids = q ^. expertUuids
        , _integrationQuestionIntegrationUuid = U.nil
        , _integrationQuestionProps = M.empty
        }

updateIntegrationProps :: EditIntegrationEvent -> Question -> Question
updateIntegrationProps e (IntegrationQuestion' q) = IntegrationQuestion' $ q & props .~ updatedProps
  where
    updatedProps =
      if q ^. integrationUuid == e ^. entityUuid
        then case e ^. props of
               ChangedValue ps -> M.fromList . fmap (\p -> (p, fromMaybe "" (M.lookup p (q ^. props)))) $ ps
               NothingChanged -> q ^. props
        else q ^. props
updateIntegrationProps _ q' = q'

deleteIntegrationReference :: DeleteIntegrationEvent -> Question -> Question
deleteIntegrationReference e (IntegrationQuestion' q) =
  if q ^. integrationUuid == e ^. entityUuid
    then convertToValueQuestion . IntegrationQuestion' $ q
    else IntegrationQuestion' q
deleteIntegrationReference _ q' = q'

deletePhaseReference :: DeletePhaseEvent -> Question -> Question
deletePhaseReference e q' =
  case q' ^. requiredPhaseUuid' of
    Just requiredPhaseUuid ->
      if requiredPhaseUuid == e ^. entityUuid
        then q' & requiredPhaseUuid' .~ Nothing
        else q'
    Nothing -> q'

deleteTagReference :: DeleteTagEvent -> Question -> Question
deleteTagReference e q = q & tagUuids' .~ L.delete (e ^. entityUuid) (q ^. tagUuids')
