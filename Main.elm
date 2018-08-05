module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, second)


main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



--MODEL


type alias Model =
    { focus : Bool
    , focusTime : Int
    , rest : Bool
    , restTime : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { focus = False
      , focusTime = 1500
      , rest = False
      , restTime = 300
      }
    , Cmd.none
    )



--UDPATE


type Msg
    = FocusTick Time
    | RestTick Time
    | Focus
    | Rest
    | Reset
    | Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusTick newTime ->
            if model.focusTime > 0 then
                ( { model
                    | focusTime = model.focusTime - 1
                  }
                , Cmd.none
                )
            else
                ( { model
                    | focus = flip model.focus
                    , rest = flip model.rest
                    , focusTime = 1500
                  }
                , Cmd.none
                )

        RestTick newTime ->
            if model.restTime > 0 then
                ( { model
                    | restTime = model.restTime - 1
                  }
                , Cmd.none
                )
            else
                ( { model
                    | focus = flip model.focus
                    , rest = flip model.rest
                    , restTime = 300
                  }
                , Cmd.none
                )

        Focus ->
            ( { model | focus = flip model.focus }, Cmd.none )

        Rest ->
            ( { model | rest = flip model.rest }, Cmd.none )

        Reset ->
            ( { model
                | focus = False
                , rest = False
                , focusTime = 1500
                , restTime = 300
              }
            , Cmd.none
            )

        Nothing ->
            ( model, Cmd.none )


flip : Bool -> Bool
flip bool =
    not bool


startStop : Model -> Msg
startStop model =
    if model.focus == True then
        Focus
    else if model.rest == True then
        Rest
    else if model.focusTime < 1500 then
        Focus
    else if model.restTime < 300 then
        Rest
    else if model.focus == False && model.rest == False then
        Focus
    else
        Nothing



--SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.focus == True then
        Time.every second FocusTick
    else if model.rest == True then
        Time.every second RestTick
    else
        Sub.none



--VIEW


view : Model -> Html Msg
view model =
    div [ class "root" ]
        [ div [ class "control" ]
            [ button [ class "button", onClick (startStop model) ]
                [ Html.text (renderStartStop model) ]
            , button [ class "button", onClick Reset ]
                [ Html.text "Reset" ]
            ]
        , div [ classList [ ( "timer", True ), ( "active", model.focus ) ] ]
            [ Html.text (renderTime model.focusTime)
            ]
        , div [ classList [ ( "timer", True ), ( "active-rest", model.rest ) ] ]
            [ Html.text (renderTime model.restTime)
            ]
        ]



-- VIEW HELPERS


zeroPad : Int -> String
zeroPad n =
    toString n |> String.padLeft 2 '0'


renderStartStop : Model -> String
renderStartStop model =
    if model.focus == True || model.rest == True then
        "Stop"
    else
        "Start"


renderTime : Int -> String
renderTime time =
    let
        minutes =
            time // 60

        seconds =
            time % 60
    in
    zeroPad minutes ++ " : " ++ zeroPad seconds
