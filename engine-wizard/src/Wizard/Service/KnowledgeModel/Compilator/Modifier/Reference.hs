module Wizard.Service.KnowledgeModel.Compilator.Modifier.Reference where

import qualified Data.UUID as U

import Shared.Model.Event.Reference.ReferenceEvent
import Shared.Model.KnowledgeModel.KnowledgeModel
import Wizard.Service.KnowledgeModel.Compilator.Modifier.Modifier

instance CreateEntity AddReferenceEvent Reference where
  createEntity (AddResourcePageReferenceEvent' event) =
    ResourcePageReference' $
      ResourcePageReference
        { uuid = event.entityUuid
        , shortUuid = event.shortUuid
        , annotations = event.annotations
        }
  createEntity (AddURLReferenceEvent' event) =
    URLReference' $
      URLReference
        { uuid = event.entityUuid
        , url = event.url
        , aLabel = event.aLabel
        , annotations = event.annotations
        }
  createEntity (AddCrossReferenceEvent' event) =
    CrossReference' $
      CrossReference
        { uuid = event.entityUuid
        , targetUuid = event.targetUuid
        , description = event.description
        , annotations = event.annotations
        }

instance EditEntity EditReferenceEvent Reference where
  editEntity event' ref =
    case event' of
      (EditResourcePageReferenceEvent' event) -> applyToResourcePageReference event . convertToResourcePageReference $ ref
      (EditURLReferenceEvent' event) -> applyToURLReference event . convertToURLReference $ ref
      (EditCrossReferenceEvent' event) -> applyToCrossReference event . convertToCrossReference $ ref
    where
      applyToResourcePageReference event resourcePageReference =
        ResourcePageReference' $
          resourcePageReference
            { shortUuid = applyValue resourcePageReference.shortUuid event.shortUuid
            , annotations = applyValue resourcePageReference.annotations event.annotations
            }
      applyToURLReference event urlReference =
        URLReference' $
          urlReference
            { url = applyValue urlReference.url event.url
            , aLabel = applyValue urlReference.aLabel event.aLabel
            , annotations = applyValue urlReference.annotations event.annotations
            }
      applyToCrossReference event crossReference =
        CrossReference' $
          crossReference
            { targetUuid = applyValue crossReference.targetUuid event.targetUuid
            , description = applyValue crossReference.description event.description
            , annotations = applyValue crossReference.annotations event.annotations
            }

convertToResourcePageReference :: Reference -> ResourcePageReference
convertToResourcePageReference (ResourcePageReference' ref) = ref
convertToResourcePageReference ref' =
  case ref' of
    (URLReference' ref) -> createQuestion ref
    (CrossReference' ref) -> createQuestion ref
  where
    createQuestion ref =
      ResourcePageReference
        { uuid = ref.uuid
        , shortUuid = ""
        , annotations = ref.annotations
        }

convertToURLReference :: Reference -> URLReference
convertToURLReference (URLReference' ref) = ref
convertToURLReference ref' =
  case ref' of
    (ResourcePageReference' ref) -> createQuestion ref
    (CrossReference' ref) -> createQuestion ref
  where
    createQuestion ref =
      URLReference
        { uuid = ref.uuid
        , url = ""
        , aLabel = ""
        , annotations = ref.annotations
        }

convertToCrossReference :: Reference -> CrossReference
convertToCrossReference (CrossReference' ref) = ref
convertToCrossReference ref' =
  case ref' of
    (ResourcePageReference' ref) -> createQuestion ref
    (URLReference' ref) -> createQuestion ref
  where
    createQuestion ref =
      CrossReference
        { uuid = ref.uuid
        , targetUuid = U.nil
        , description = ""
        , annotations = ref.annotations
        }
