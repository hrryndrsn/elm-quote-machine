module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, at, field, int, list, map, map3, string)
import Random exposing (..)
import Random.Extra exposing (..)
import Task


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--MODEL


type alias Quote =
    { quote : String
    , author : String
    }


type alias QuoteList =
    List Quote


type alias Model =
    { quotes : List Quote
    , selectedQuote : Maybe Quote
    , quotesLoaded : Bool
    , styleBool : Bool
    , error : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] Nothing False False "", getData )



--UPDATE


type Msg
    = FetchData
    | NewData (Result Http.Error QuoteList)
    | Generate
    | RandomQuote (Maybe Quote)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchData ->
            ( model, getData )

        NewData (Ok data) ->
            ( { model
                | quotes = data
                , quotesLoaded = True
              }
            , message Generate
            )

        NewData (Err err) ->
            ( { model | error = toString err }, Cmd.none )

        Generate ->
            ( model, Random.generate RandomQuote (chooseRandomQuote model.quotes) )

        RandomQuote newQuote ->
            ( { model
                | selectedQuote = newQuote
                , styleBool = flip model.styleBool
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


flip : Bool -> Bool
flip bool =
    not bool



--VIEW


view : Model -> Html Msg
view model =
    div []
        [ div
            []
            [ renderQuote model.selectedQuote model.styleBool
            ]
        ]



--HELPERS


renderQuote : Maybe Quote -> Bool -> Html Msg
renderQuote quote bool =
    case quote of
        Nothing ->
            div []
                [ text "loading" ]

        Just a ->
            div [ class "container" ]
                [ div [ class (renderQuoteClass bool) ] [ text a.quote ]
                , div [ class (renderAuthorClass bool) ]
                    [ div [] [ text a.author ]
                    , div []
                        [ button [ class "generate", onClick Generate ] [ text "New quote" ]
                        ]
                    ]
                ]


renderAuthorClass : Bool -> String
renderAuthorClass bool =
    let
        style =
            toString bool
    in
    "sub " ++ style


renderQuoteClass : Bool -> String
renderQuoteClass bool =
    let
        style =
            toString bool
    in
    "quote " ++ style


chooseRandomQuote : List Quote -> Generator (Maybe Quote)
chooseRandomQuote quotes =
    Random.Extra.sample quotes



--turn a messgae in to a Cmd Msg (Hackery)


message : msg -> Cmd msg
message msg =
    Task.perform identity (Task.succeed msg)



--HTTP


quoteDecoder : Decoder Quote
quoteDecoder =
    Json.Decode.map2 Quote
        (field "quote" string)
        (field "author" string)


decodeData : Decoder QuoteList
decodeData =
    Json.Decode.at [ "quotes" ] (Json.Decode.list quoteDecoder)


getData : Cmd Msg
getData =
    let
        url =
            "https://gist.githubusercontent.com/camperbot/5a022b72e96c4c9585c32bf6a75f62d9/raw/e3c6895ce42069f0ee7e991229064f167fe8ccdc/quotes.json"

        request =
            Http.get url decodeData
    in
    Http.send NewData request



--SUBCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
