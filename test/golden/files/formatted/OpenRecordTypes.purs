module OpenRecordTypes where

foreign import merge :: forall r1 r2 u. Union r1 r2 u => { | r1 } -> { | r2 } -> { | u }
