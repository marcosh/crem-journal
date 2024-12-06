module Gen.JournalEntryContent where

import "hedgehog" Hedgehog (Gen)
import "hedgehog" Hedgehog.Gen (alphaNum, mapMaybe, text)
import "hedgehog" Hedgehog.Range (constant)

import "crem-journal" Domain.Write.JournalEntryContent (JournalEntryContent, journalEntryContent)

genJournalEntryContent :: Gen JournalEntryContent
genJournalEntryContent = mapMaybe (either (const Nothing) Just . journalEntryContent) $ text (constant 1 10) alphaNum
