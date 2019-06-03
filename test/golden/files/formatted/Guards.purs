module Guards where

altAppend ::
  forall a.
  NonEmptyArray (RouteParser a) ->
  NonEmptyArray (RouteParser a) ->
  NonEmptyArray (RouteParser a)
altAppend as bs
  | Segment pre a <- NEA.last as
  , Segment pre' b <- NEA.head bs
  , pre == pre' =
    let
      bs' = NEA.cons' (Segment pre (a <|> b)) (NEA.tail bs)
    in
      case NEA.fromArray (NEA.init as) of
        Just as' -> as' `altAppend` bs'
        Nothing -> bs'
  | otherwise = as <> bs
