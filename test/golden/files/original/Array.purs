module Array where
f :: Array { x :: Int , y :: Boolean}
f = [
 {x : 1, y:
 true},
 {x:10,y:false},{x:0,
 y:                true}]

g :: Array Boolean
g =
  [(
   true || false && true),
   (true
   || true
   && true
   || false)]

h :: Array Int
h = [ 1,2,3,4,5,6]

i :: Array String
i = ["hello", """
there""","this","""
              is quite the `Array String` """]
