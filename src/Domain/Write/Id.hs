module Domain.Write.Id where

import "uuid" Data.UUID (UUID)

newtype Id a = Id UUID
