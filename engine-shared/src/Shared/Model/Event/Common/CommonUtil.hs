module Shared.Model.Event.Common.CommonUtil where

class IsEmptyEvent event where
  isEmptyEvent :: event -> Bool
