module Service.Migration.KnowledgeModel.Applicator.Utils where

hFoldl fn acc values callback =
  case foldl fn acc values of
    Left error -> Left error
    Right res -> callback res
