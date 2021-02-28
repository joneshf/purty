module KindSignature where

class Foo :: Type -> Constraint
class Foo a
data Bar :: Type -> Type
data Bar a = Bar
newtype Baz :: Type ->
 Type
newtype Baz a = Baz (Bar a)
type Qux :: forall a. a -> (a -> a) ->
            Type
type Qux a b = Int
