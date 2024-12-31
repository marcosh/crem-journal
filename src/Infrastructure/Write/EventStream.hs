module Infrastructure.Write.EventStream
  ( EventStream
  )
where

-- | An event stream is an ordered list of events
newtype EventStream a = EventStream [a]
  deriving newtype (Functor, Foldable)
