module Main exposing (..)

import Haggis
import Html
import Html.App
import Html.Attributes


type alias Model =
    { is_on : Bool }


initialize =
    { is_on = True }


update msg model =
    model


view model =
    Html.div no_attributes
        [ Html.div
            [ Html.Attributes.style
                [ ( "background-color", "orange" )
                , ( "width", "80px" )
                , ( "height", "80px" )
                , ( "border-radius", "6px" )
                , ( "margin", "4px" )
                ]
            ]
            no_element
        , Html.hr no_attributes no_element
        , Html.pre no_attributes [ Html.text <| toString model ]
        ]


no_attributes =
    []


no_element =
    []


main =
    Html.App.beginnerProgram
        { model = initialize
        , update = update
        , view = view
        }
