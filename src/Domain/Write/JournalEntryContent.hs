module Domain.Write.JournalEntryContent (JournalEntryContent, journalEntryContent) where

import "text" Data.Text (Text, null, strip)
import "base" Prelude hiding (null)

-- | The content of a journal entry. Should be a non-empty string after trimming
newtype JournalEntryContent = JournalEntryContent Text

-- | Ways in which a text can fail to become a journal entry content
data JournalEntryContentError
  = EmptyAfterTrimming

-- | Try to create a @JournalEntryContent from a @Text
-- If could fail with a @JournalEntryContentError
journalEntryContent :: Text -> Either JournalEntryContentError JournalEntryContent
journalEntryContent text =
  if null (strip text)
    then Left EmptyAfterTrimming
    else Right $ JournalEntryContent text
