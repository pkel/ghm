module Helpers.List exposing
    ( minimumNotNothing
    , maximumNotNothing
    )

minimumNotNothing : List (Maybe comparable) -> Maybe comparable
minimumNotNothing l =
    let min a b =
            case (a, b) of
                ( Nothing, b ) -> b
                ( a, Nothing ) -> a
                ( Just a, Just b ) -> List.minimum [a,b]
    in
        List.foldl min Nothing l

maximumNotNothing : List (Maybe comparable) -> Maybe comparable
maximumNotNothing l =
    let max a b =
            case (a, b) of
                ( Nothing, b ) -> b
                ( a, Nothing ) -> a
                ( Just a, Just b ) -> List.maximum [a,b]
    in
        List.foldl max Nothing l

