Elm-Package for human-friendly sorting of lists.

```elm
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
    , ul [] <| List.map (\txt -> li [] [ text txt ]) defaultSort
    , h3 [] [ text "Natural Sort" ]
    , ul [] <| List.map (\txt -> li [] [ text txt ]) naturalSort
    ]
```

The snippet above will output

```
# Default Sort

* 1-orange
* 12-apple
* 2-bananb
* 4-banana

# Natural Sort

* 1-orange
* 2-bananb
* 4-banana
* 12-apple
```
