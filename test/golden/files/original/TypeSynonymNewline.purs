module TypeSynonymNewline where

type T
  = {foo :: Int, bar :: {baz :: Int, qux :: {lhs :: Int, rhs :: Int}}}

type U
  = {foo :: Int, bar :: {baz :: Int, qux ::
                            {lhs :: Int, rhs :: Int}}}

type V v
  = { foo :: Int, bar :: {baz :: Int, qux ::
                            {lhs :: Int, rhs :: Int}} |
v }

t :: T
t = { foo: 1 , bar: { baz: 2 , qux: { lhs: 3 , rhs: 4}}}

u :: T
u =
  { foo: 1 , bar: { baz: 2 , qux:
      { lhs: 3 , rhs: 4}}}
