module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as App
import Matrix exposing (..)
import Matrix.Extra exposing (..)
import Array
import Time exposing (Time, second)


-- # Main
-- main : App.program


main =
    App.program
        { init = initBar
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- # Model


type alias Model =
    { isOn : Matrix Bool }



--init : Model


init =
    { isOn = Matrix.repeat 6 5 False }


initBar =
    let
        x =
            Matrix.indexedMap
                (\x y value ->
                    if x == 0 || x == 2 then
                        True
                    else
                        False
                )
                init.isOn
    in
        ( { isOn = x }, Cmd.none )


isSolved : Model -> Bool
isSolved model =
    model.isOn
        |> Matrix.filter identity
        |> Array.isEmpty



-- # Messages


type Msg
    = Toggle LightIndex
    | Step
    | Restart
    | Tick Time


type alias LightIndex =
    { x : Int, y : Int }



-- # Update


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Toggle index ->
            ( { model | isOn = toggleLights index model.isOn }, Cmd.none )

        Step ->
            ( { model | isOn = step model.isOn }, Cmd.none )

        Tick time ->
            ( { model | isOn = step model.isOn }, Cmd.none )

        Restart ->
            ( init, Cmd.none )


toggleLights : LightIndex -> Matrix Bool -> Matrix Bool
toggleLights index cells =
    Matrix.indexedMap
        (\x y cell ->
            if x == index.x && y == index.y then
                not cell
            else
                cell
        )
        cells


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick


step : Matrix Bool -> Matrix Bool
step isOn =
    Matrix.indexedMap
        (\x y value ->
            let
                ns =
                    Matrix.Extra.neighbours x y isOn

                live_ns =
                    List.foldr oneiftrue 0 ns

                _ =
                    Debug.log "live_ns" live_ns
            in
                if value then
                    if live_ns == 2 || live_ns == 3 then
                        True
                    else if live_ns > 3 then
                        False
                    else if live_ns < 2 then
                        False
                    else
                        False
                else if live_ns == 3 then
                    True
                else
                    False
        )
        isOn


oneiftrue flag sum =
    if flag then
        sum + 1
    else
        sum



-- # View


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ model.isOn
            |> Matrix.indexedMap btn
            |> toDivs
        , winScreen model
        , hr [] []
        , Html.button
            [ onClick Restart ]
            [ text "Restart" ]
        , Html.button
            [ onClick Step ]
            [ text "Step" ]
        , hr [] []
        , div []
            [ model.isOn
                |> Matrix.Extra.prettyPrint
            ]
        ]


winScreen : Model -> Html Msg
winScreen model =
    if isSolved model then
        h1 []
            [ text "Win!!" ]
    else
        div [] []


toDivs : Matrix (Html Msg) -> Html Msg
toDivs matrix =
    let
        row y =
            Matrix.getRow y matrix
                |> Maybe.map Array.toList
                |> Maybe.withDefault []
                |> div []

        height =
            Matrix.height matrix
    in
        [0..height]
            |> List.map row
            |> div []


btn : Int -> Int -> Bool -> Html Msg
btn ix iy isOn =
    div
        [ style
            [ ( "background-color"
              , if isOn then
                    "orange"
                else
                    "grey"
              )
            , ( "width", "80px" )
            , ( "height", "80px" )
            , ( "margin", "2px" )
            , ( "border-radius", "10px" )
            , ( "display", "inline-block" )
            ]
        , onClick (Toggle { x = ix, y = iy })
        ]
        []
