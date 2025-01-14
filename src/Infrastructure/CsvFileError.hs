module Infrastructure.CsvFileError where

data CsvFileError a
  = UnableToParseCsvFile String
  | CsvContentError a
