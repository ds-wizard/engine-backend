module KMMigration.Migration.Event.EventApplicable where

import KMMigration.Migration.Applicator.Applicator

data EventApplicable =
  forall a. ApplyEventToKM a =>
            MkEventApplicable a
