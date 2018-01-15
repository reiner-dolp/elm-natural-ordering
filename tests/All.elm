module All exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import NaturalOrdering exposing (defaultNaturalOrderingOptions)
import Test exposing (..)


suite : Test
suite =
    describe "Natural Sorting"
        [ describe "Basic Functionality"
            [ fuzz string "Correctly sorts when starting with number" <|
                \str ->
                    let
                        lower =
                            "1a" ++ str

                        higher =
                            "12b" ++ str
                    in
                    [ higher, lower ]
                        |> NaturalOrdering.sort
                        |> Expect.equal [ lower, higher ]
            ]
        , describe "Options"
            [ test "Option lettersBeforeNumbers = True" <|
                \_ ->
                    let
                        mySort =
                            NaturalOrdering.customSort { defaultNaturalOrderingOptions | lettersBeforeNumbers = True }
                    in
                    mySort [ "1", "a" ] |> Expect.equal [ "a", "1" ]
            , test "Option lettersBeforeNumbers = False" <|
                \_ ->
                    let
                        mySort =
                            NaturalOrdering.customSort { defaultNaturalOrderingOptions | lettersBeforeNumbers = False }
                    in
                    mySort [ "1", "a" ] |> Expect.equal [ "1", "a" ]
            , test "Option caseInsensitive = True" <|
                \str ->
                    [ "ab", "Ac" ]
                        |> NaturalOrdering.customSort { defaultNaturalOrderingOptions | caseInsensitive = True }
                        |> Expect.equal [ "ab", "Ac" ]
            ]
        , describe "Sort by Property"
            [ fuzz string "is stable" <|
                \str ->
                    let
                        first =
                            { marker = 1
                            , value = str
                            }

                        second =
                            { marker = 2
                            , value = str
                            }
                    in
                    [ first, second ]
                        |> NaturalOrdering.sortBy .value
                        |> Expect.equal [ first, second ]
            , fuzz string "case insensitive is stable" <|
                \str ->
                    let
                        first =
                            { marker = 1
                            , value = String.toUpper str
                            }

                        second =
                            { marker = 2
                            , value = String.toLower str
                            }

                        caseinsensitiveSort =
                            NaturalOrdering.customSort { defaultNaturalOrderingOptions | caseInsensitive = True }
                    in
                    [ first, second ]
                        |> NaturalOrdering.sortBy .value
                        |> Expect.equal [ first, second ]
            ]
        , describe "Larger Examples"
            [ test "README Example" <|
                \_ ->
                    let
                        input =
                            [ "4-banana"
                            , "12-apple"
                            , "1-orange"
                            , "2-bananb"
                            ]

                        expected =
                            [ "1-orange"
                            , "2-bananb"
                            , "4-banana"
                            , "12-apple"
                            ]

                        actual =
                            NaturalOrdering.sort input
                    in
                    Expect.equal actual expected
            ]
        ]
