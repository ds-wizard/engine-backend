module Wizard.Database.BSON.Document.DocumentType where

import qualified Data.Bson as BSON

import Wizard.Model.Document.Document

instance BSON.Val DocumentState where
  val QueuedDocumentState = BSON.String "QueuedDocumentState"
  val InProgressDocumentState = BSON.String "InProgressDocumentState"
  val DoneDocumentState = BSON.String "DoneDocumentState"
  val ErrorDocumentState = BSON.String "ErrorDocumentState"
  cast' (BSON.String "QueuedDocumentState") = Just QueuedDocumentState
  cast' (BSON.String "InProgressDocumentState") = Just InProgressDocumentState
  cast' (BSON.String "DoneDocumentState") = Just DoneDocumentState
  cast' (BSON.String "ErrorDocumentState") = Just ErrorDocumentState
  cast' _ = Nothing

instance BSON.Val DocumentDurability where
  val PersistentDocumentDurability = BSON.String "PersistentDocumentDurability"
  val TemporallyDocumentDurability = BSON.String "TemporallyDocumentDurability"
  cast' (BSON.String "PersistentDocumentDurability") = Just PersistentDocumentDurability
  cast' (BSON.String "TemporallyDocumentDurability") = Just TemporallyDocumentDurability
  cast' _ = Nothing
