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

j :: Array (Array (Array Int))
j = [[[1,2,3,4,5],[6,7]],[[8],
                          [9,10]],[[11
                                   ,12       ,13], 14]]

k :: Array (Array Int)
k = [identity [1,2,
               3], [4
                   ,5,6]
    `const` [7
            ,8]]
