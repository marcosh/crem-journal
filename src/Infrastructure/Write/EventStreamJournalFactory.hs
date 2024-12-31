module Infrastructure.Write.EventStreamJournalFactory
  ( reconstructJournal
  )
where

import "base" Data.Foldable (foldl')

import Domain.Write.Journal (Journal, applyJournalEvent)
import Domain.Write.JournalEvent (JournalEvent)
import Infrastructure.Write.EventStream (EventStream)

reconstructJournal :: EventStream JournalEvent -> Journal
reconstructJournal =
  foldl' applyJournalEvent mempty
