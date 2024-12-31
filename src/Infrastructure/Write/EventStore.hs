module Infrastructure.Write.EventStore where

import Domain.Write.JournalEvent (JournalEvent)
import Infrastructure.Write.EventStream (EventStream)

class EventStore m where
  storeEvent :: JournalEvent -> m ()
  retrieveEvents :: m (EventStream JournalEvent)
