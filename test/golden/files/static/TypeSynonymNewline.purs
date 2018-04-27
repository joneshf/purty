module TypeSynonymNewline where

type T
  = { foo :: Int, bar :: { baz :: Int, qux :: { lhs :: Int, rhs :: Int } } }

init ::
  T
init = { foo: 1
       , bar: { baz: 2
              , qux: { lhs: 3
                     , rhs: 4
                     }
              }
       }
