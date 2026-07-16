module Wizard.Service.KnowledgeModel.Locale.Pot.PotFileService where

import Control.Monad.Reader (liftIO)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import qualified Data.UUID as U

import Shared.Common.Util.String (f', trim)
import Shared.KnowledgeModel.Database.DAO.Package.KnowledgeModelPackageDAO
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModel
import Shared.KnowledgeModel.Model.KnowledgeModel.KnowledgeModelLenses
import Shared.KnowledgeModel.Model.KnowledgeModel.Package.KnowledgeModelPackage
import Wizard.Database.DAO.Common
import Wizard.Model.Context.AclContext
import Wizard.Model.Context.AppContext
import Wizard.Model.Context.AppContextHelpers
import Wizard.Service.KnowledgeModel.KnowledgeModelService
import Wizard.Util.Gettext
import WizardLib.Public.Api.Resource.TemporaryFile.TemporaryFileDTO
import qualified WizardLib.Public.Service.TemporaryFile.TemporaryFileMapper as TemporaryFileMapper
import WizardLib.Public.Service.TemporaryFile.TemporaryFileService

getTemporaryFileWithTranslationTemplate :: U.UUID -> AppContextM TemporaryFileDTO
getTemporaryFileWithTranslationTemplate pkgUuid =
  runInTransaction $ do
    checkPermission _KNOWLEDGE_MODELS_MANAGE_ROLE_PERMISSION
    pkg <- findPackageByUuid pkgUuid
    knowledgeModel <- compileKnowledgeModel [] (Just pkgUuid) []
    now <- liftIO getCurrentTime
    let pot = buildTranslationTemplate pkg knowledgeModel now
    mCurrentUserUuid <- getCurrentUserUuid
    let fileName = f' "%s_%s_%s.pot" [pkg.organizationId, pkg.kmId, pkg.version]
    url <- createTemporaryFile fileName "application/octet-stream" mCurrentUserUuid (BSL.fromStrict . TE.encodeUtf8 . T.pack $ pot)
    return $ TemporaryFileMapper.toDTO url "application/octet-stream"

buildTranslationTemplate :: KnowledgeModelPackage -> KnowledgeModel -> UTCTime -> String
buildTranslationTemplate pkg km now = serializePot headers (buildPotEntries km)
  where
    headers =
      [ ("Project-Id-Version", f' "%s:%s:%s" [pkg.organizationId, pkg.kmId, pkg.version])
      , ("POT-Creation-Date", formatTime defaultTimeLocale "%Y-%m-%d %H:%M%z" now)
      , ("MIME-Version", "1.0")
      , ("Content-Type", "text/plain; charset=UTF-8")
      , ("Content-Transfer-Encoding", "8bit")
      , ("Language", pkg.language)
      , ("Plural-Forms", pluralForms pkg.language)
      ]

buildPotEntries :: KnowledgeModel -> [PotEntry]
buildPotEntries km =
  dedupeEntries $
    concatMap chapterEntries (resolve km.chapterUuids km.entities.chapters)
      ++ concatMap metricEntries (resolve km.metricUuids km.entities.metrics)
      ++ concatMap phaseEntries (resolve km.phaseUuids km.entities.phases)
      ++ concatMap tagEntries (resolve km.tagUuids km.entities.tags)
      ++ concatMap resourceCollectionEntries (resolve km.resourceCollectionUuids km.entities.resourceCollections)
  where
    chapterEntries :: Chapter -> [PotEntry]
    chapterEntries chapter =
      entry "chapter" chapter.uuid "title" chapter.title
        ++ maybeEntry "chapter" chapter.uuid "text" chapter.text
        ++ concatMap questionEntries (resolve chapter.questionUuids km.entities.questions)
    questionEntries :: Question -> [PotEntry]
    questionEntries question =
      entry "question" (getUuid question) "title" (getTitle question)
        ++ maybeEntry "question" (getUuid question) "text" (getText question)
        ++ concatMap answerEntries (resolve (getAnswerUuids question) km.entities.answers)
        ++ concatMap choiceEntries (resolve (getChoiceUuids question) km.entities.choices)
        ++ concatMap questionEntries (resolve (getItemTemplateQuestionUuids question) km.entities.questions)
        ++ concatMap referenceEntries (resolve (getReferenceUuids question) km.entities.references)
    answerEntries :: Answer -> [PotEntry]
    answerEntries answer =
      entry "answer" answer.uuid "label" answer.aLabel
        ++ maybeEntry "answer" answer.uuid "advice" answer.advice
        ++ concatMap questionEntries (resolve answer.followUpUuids km.entities.questions)
    choiceEntries :: Choice -> [PotEntry]
    choiceEntries choice = entry "choice" choice.uuid "label" choice.aLabel
    referenceEntries :: Reference -> [PotEntry]
    referenceEntries (URLReference' reference) = entry "reference" reference.uuid "label" reference.aLabel
    referenceEntries (CrossReference' reference) = entry "reference" reference.uuid "description" reference.description
    referenceEntries (ResourcePageReference' _) = []
    metricEntries :: Metric -> [PotEntry]
    metricEntries metric =
      entry "metric" metric.uuid "title" metric.title
        ++ maybeEntry "metric" metric.uuid "abbreviation" metric.abbreviation
        ++ maybeEntry "metric" metric.uuid "description" metric.description
    phaseEntries :: Phase -> [PotEntry]
    phaseEntries phase =
      entry "phase" phase.uuid "title" phase.title
        ++ maybeEntry "phase" phase.uuid "description" phase.description
    tagEntries :: Tag -> [PotEntry]
    tagEntries tag =
      entry "tag" tag.uuid "name" tag.name
        ++ maybeEntry "tag" tag.uuid "description" tag.description
    resourceCollectionEntries :: ResourceCollection -> [PotEntry]
    resourceCollectionEntries collection =
      entry "resourceCollection" collection.uuid "title" collection.title
        ++ concatMap resourcePageEntries (resolve collection.resourcePageUuids km.entities.resourcePages)
    resourcePageEntries :: ResourcePage -> [PotEntry]
    resourcePageEntries page =
      entry "resourcePage" page.uuid "title" page.title
        ++ entry "resourcePage" page.uuid "content" page.content

resolve :: [U.UUID] -> M.Map U.UUID entity -> [entity]
resolve uuids entityMap = mapMaybe (`M.lookup` entityMap) uuids

entry :: String -> U.UUID -> String -> String -> [PotEntry]
entry entityType entityUuid fieldName value
  | null (trim value) = []
  | otherwise = [PotEntry {references = [f' "%s/%s/%s" [entityType, U.toString entityUuid, fieldName]], msgid = value}]

maybeEntry :: String -> U.UUID -> String -> Maybe String -> [PotEntry]
maybeEntry entityType entityUuid fieldName = maybe [] (entry entityType entityUuid fieldName)

dedupeEntries :: [PotEntry] -> [PotEntry]
dedupeEntries = L.foldl' step []
  where
    step acc newEntry =
      case L.findIndex (\e -> e.msgid == newEntry.msgid) acc of
        Just i ->
          case splitAt i acc of
            (before, existing : after) -> before ++ ((existing {references = existing.references ++ newEntry.references} :: PotEntry) : after)
            _ -> acc
        Nothing -> acc ++ [newEntry]
