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
    { isOn : Matrix Bool, started : Bool }



--init : Model


init =
    { isOn = Matrix.repeat 12 11 False, started = True }


initBar =
    let
        x =
            Matrix.indexedMap
                (\x y value ->
                    if
                        (x == 6 && y == 3)
                            || (x == 6 && y == 4)
                            || (x == 6 && y == 5)
                            || (x == 5 && y == 4)
                            || (x == 7 && y == 4)
                    then
                        True
                    else
                        False
                )
                init.isOn
    in
        ( { isOn = x, started = True }, Cmd.none )


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
    | Start
    | Pause


type alias LightIndex =
    { x : Int, y : Int }



-- # Update


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Toggle index ->
            ( { model | isOn = toggleLights index model.isOn, started = False }, Cmd.none )

        Step ->
            ( { model | isOn = step model.isOn }, Cmd.none )

        Tick time ->
            ( { model | isOn = step model.isOn }, Cmd.none )

        Restart ->
            initBar

        Start ->
            ( { model | started = True }, Cmd.none )

        Pause ->
            ( { model | started = False }, Cmd.none )


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
    if model.started then
        Time.every second Tick
    else
        Sub.none


step : Matrix Bool -> Matrix Bool
step isOn =
    Matrix.indexedMap
        (\x y value ->
            let
                ns =
                    Matrix.Extra.neighbours x y isOn

                live_ns =
                    List.foldr oneiftrue 0 ns

                -- _ =
                -- Debug.log "live_ns" live_ns
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
        [ h2 [] [ text "Game Of Life in ELM" ]
        , p [ class "quiet" ] [ text "Click on the cells to toggle and press 'Start' to start simulation." ]
        , Html.button
            [ onClick Restart ]
            [ text "Restart" ]
        , Html.text " "
        , start_pause model
        , Html.text "  "
        , Html.button
            [ onClick Step ]
            [ text "Step" ]
        , hr [] []
        , model.isOn
            |> Matrix.indexedMap btn
            |> toDivs
        , hr [] []
        , div []
            [ model.isOn
                |> Matrix.Extra.prettyPrint
            ]
        ]


start_pause : Model -> Html Msg
start_pause model =
    if model.started then
        Html.button
            [ style
                [ ( "background-color", "red" )
                , ( "color", "white" )
                , ( "font-style", "bold" )
                , ( "display", "inline-block" )
                ]
            , onClick Pause
            ]
            [ text "Pause" ]
    else
        Html.button
            [ style
                [ ( "background-color", "green" )
                , ( "color", "white" )
                , ( "font-style", "bold" )
                , ( "display", "inline-block" )
                ]
            , onClick Start
            ]
            [ text "Start" ]


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
                    "black"
                else
                    "grey"
              )
            , ( "width", "40px" )
            , ( "height", "40px" )
            , ( "margin", "1px" )
            , ( "display", "inline-block" )
            ]
        , onClick (Toggle { x = ix, y = iy })
        ]
        []
