module NaturalOrdering exposing (Options, customSort, customSortBy, defaultNaturalOrderingOptions, defaultOptions, sort, sortBy)

{-| Sorts strings in a [human-friendly, natural ordering](https://en.wikipedia.org/wiki/Natural_sort_order)
by treating multi-digit numbers as a single character.


# Usage Example

    import Html exposing (div, h3, li, text, ul)
    import NaturalOrdering

    main =
        let
            data =
                [ "4-banana"
                , "12-apple"
                , "1-orange"
                , "2-bananb"
                ]

            defaultSort =
                List.sort data

            naturalSort =
                sort data
        in
        div []
            [ h3 [] [ text "Default Sort" ]
            , ul [] <|
                List.map
                    (\txt -> li [] [ text txt ])
                    defaultSort
            , h3 [] [ text "Natural Sort" ]
            , ul [] <|
                List.map
                    (\txt -> li [] [ text txt ])
                    naturalSort
            ]

The snippet above will output

    Default Sort

        1-orange
        12-apple
        2-bananb
        4-banana

    Natural Sort

        1-orange
        2-bananb
        4-banana
        12-apple


# Sorting

@docs sort
@docs sortBy


# Sorting With Options

@docs Options
@docs defaultOptions

@docs customSort
@docs customSortBy

@docs defaultNaturalOrderingOptions

-}

import Regex exposing (HowMany(..), regex)


type StringChunk
    = Str String
    | Num String Int


{-| Can be used for modified sorting using `customSort` and `customSortBy`.
-}
type alias Options =
    { lettersBeforeNumbers : Bool
    , caseInsensitive : Bool
    }


type alias Chunked a =
    ( a, List StringChunk )


{-| Default settings used for sorting. `lettersBeforeNumbers = False` and
`caseInsensitive = False`.
-}
defaultOptions : Options
defaultOptions =
    Options False False


{-| Alias for `defaultOptions` that can be directly imported without collision,
which is convenient if you want to use the update record syntax.

        -- this does not work in Elm 0.18
        -- mySettings = { NaturalOrdering.defaultOptions | caseInsensitive = True }

        -- instead
        mySettings =
            { defaultNaturalOrderingOptions | caseInsensitive = True }

-}
defaultNaturalOrderingOptions : Options
defaultNaturalOrderingOptions =
    defaultOptions


{-| Naturaly sort string values from lowest to highest.
-}
sort : List String -> List String
sort =
    sortBy identity



--sortWith fn selector list =
--list
--|> List.map (toChunked selector)
--|> List.sortWith fn
--|> List.map Tuple.first


{-| Naturaly sort strings from lowest to highest with non-default settings.
-}
customSort : Options -> List String -> List String
customSort opts =
    customSortBy opts identity


{-| Naturaly sort string values from lowest to highest by a derived property.
-}
sortBy : (a -> String) -> List a -> List a
sortBy =
    customSortBy defaultOptions


{-| Naturaly sort string values from lowest to highest by a derived property
using non-default settings.
-}
customSortBy : Options -> (a -> String) -> List a -> List a
customSortBy opts selector list =
    list
        |> List.map (toChunked selector)
        |> Debug.log "chunked"
        |> List.sortWith (sortChunked opts)
        |> List.map Tuple.first


toChunked : (a -> String) -> a -> Chunked a
toChunked selector obj =
    ( obj
    , Regex.find All (regex "^([0-9]+)|([^0-9]+)$") (selector obj)
        |> List.map
            (\{ submatches } ->
                case submatches of
                    [ _, Just chunk ] ->
                        Str chunk

                    [ Just chunk, _ ] ->
                        Result.withDefault (Str chunk)
                            (String.toInt chunk |> Result.map (Num chunk))

                    _ ->
                        Debug.log "unexpected regex result" <| Str ""
            )
    )


sortChunked : Options -> Chunked a -> Chunked a -> Order
sortChunked opts ( _, astr ) ( _, bstr ) =
    List.map2
        (\a b ->
            case Debug.log "in" ( a, b ) of
                ( Num _ a, Num _ b ) ->
                    Debug.log "out" <| orderFromSign (a - b)

                ( Str a, Num b _ ) ->
                    Debug.log "out" <| orderFromBool (not opts.lettersBeforeNumbers)

                ( Num a _, Str b ) ->
                    Debug.log "out" <| orderFromBool opts.lettersBeforeNumbers

                ( Str a, Str b ) ->
                    Debug.log "out" <|
                        if opts.caseInsensitive then
                            orderFromBool (String.toLower a > String.toLower b)
                        else
                            orderFromBool (a > b)
        )
        astr
        bstr
        |> List.foldr
            (\curr res ->
                case ( curr, res ) of
                    ( _, Just _ ) ->
                        res

                    ( EQ, _ ) ->
                        Nothing

                    _ ->
                        Just curr
            )
            Nothing
        |> Maybe.withDefault EQ


orderFromSign : Int -> Order
orderFromSign a =
    if a > 0 then
        GT
    else if a < 0 then
        LT
    else
        EQ


orderFromBool : Bool -> Order
orderFromBool a =
    if a then
        LT
    else
        GT
