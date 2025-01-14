module Infrastructure.Write.EventStream
  ( EventStream
  , fromEventList
  )
where

import Data.List (sort)

-- | An event stream is an ordered list of events
newtype EventStream a = EventStream [a]
  deriving newtype (Functor, Foldable)

fromEventList :: (Ord a) => [a] -> EventStream a
fromEventList = EventStream . sort
