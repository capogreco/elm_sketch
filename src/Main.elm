module Main exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Canvas exposing (..)
import Canvas.Settings exposing (..)
import Canvas.Settings.Advanced exposing (..)
import Color
import Html exposing (Html, div)
import Html.Attributes exposing (style)


type alias Model =
    { frameCount : Float
    , width : Float
    , height : Float
    }


type alias Flags =
    { innerWidth : Float
    , innerHeight : Float
    }


type Msg
    = Frame Float


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        c =
            0

        w =
            flags.innerWidth

        h =
            flags.innerHeight
    in
    ( Model c w h, Cmd.none )


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> onAnimationFrameDelta Frame
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update =
    \msg model ->
        case msg of
            Frame _ ->
                ( { model | frameCount = model.frameCount + 1 }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "margin" "0"
        , style "overflow" "hidden"
        ]
        [ Canvas.toHtml
            ( floor model.width, floor model.height )
            []
            [ clearScreen model
            , render model
            ]
        ]


clearScreen model =
    shapes [ fill Color.white ] [ rect ( 0, 0 ) model.width model.height ]


modByF : Float -> Float -> Float
modByF m f =
    let
        intM =
            floor m

        intF =
            floor f

        d =
            f - toFloat intF

        modI =
            modBy intM intF

        modF =
            toFloat modI
    in
    modF + d


countToHue : Float -> Float
countToHue c =
    let
        m : number
        m =
            200

        modC =
            modByF m c
    in
    modC / m


render model =
    let
        size =
            model.width / 3

        x =
            -(size / 2)

        y =
            -(size / 2)

        centerX =
            model.width / 2

        centerY =
            model.height / 2
    in
    shapes
        [ transform
            [ translate centerX centerY
            , rotate (degrees (model.frameCount * 0.1))
            ]
        , fill (Color.hsl (countToHue model.frameCount) 1 0.5)
        ]
        [ rect ( x, y ) size size ]
