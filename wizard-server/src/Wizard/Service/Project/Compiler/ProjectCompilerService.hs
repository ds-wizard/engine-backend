module Wizard.Service.Project.Compiler.ProjectCompilerService where

import qualified Data.Map.Strict as M

import Wizard.Model.Project.Event.ProjectEventList
import Wizard.Model.Project.ProjectContent
import Wizard.Model.Project.ProjectContentDM
import Wizard.Service.Project.Event.ProjectEventMapper

compileProjectEvents :: [ProjectEventList] -> ProjectContent
compileProjectEvents = foldl applyEvent defaultProjectContent

applyEvent :: ProjectContent -> ProjectEventList -> ProjectContent
applyEvent projectContent (SetReplyEventList' event) = projectContent {replies = M.insert event.path (toReply' event) projectContent.replies}
applyEvent projectContent (ClearReplyEventList' event) = projectContent {replies = M.delete event.path projectContent.replies}
applyEvent projectContent (SetPhaseEventList' event) = projectContent {phaseUuid = event.phaseUuid}
applyEvent projectContent (SetLabelsEventList' event) =
  projectContent
    { labels = case event.value of
        [] -> M.delete event.path projectContent.labels
        newValue -> M.insert event.path newValue projectContent.labels
    }
