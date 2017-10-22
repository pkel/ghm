module Url exposing (
    customer_id,
    Params,
    url
    )

import UrlParser exposing ((<?>))
import Navigation exposing (Location)

type alias Params =
    { customer_id : Maybe Int }

customer_id : Location -> Maybe Int
customer_id loc =
    let p = UrlParser.top <?> (UrlParser.intParam "customer_id")
    in
        case UrlParser.parseHash p loc of
            Just (Just id) -> Just id
            _ -> Nothing

url : Params -> Location -> String
url p loc =
    -- TODO: Only tested for file://
    let search = case p.customer_id of
        Just id -> "?customer_id=" ++ toString id
        Nothing -> ""
    in
    loc.origin ++ loc.pathname ++ search ++ loc.hash



