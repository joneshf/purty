module OpenRecordTypes where

foo ::
  forall x.
  {
    | x
  }
foo = ?foo

foreign import merge :: forall r1 r2 u. Union r1 r2 u => { | r1} -> { | r2} -> { | u}
