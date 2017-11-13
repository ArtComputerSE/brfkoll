module Main exposing (..)

import FormatNumber exposing (format)
import Html exposing (Html, div, input, label, text)
import Html.Attributes exposing (type_)
import Html.Events exposing (onInput)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { eget_kapital : String
    , långfristiga_skulder : String
    , andelstal : String
    , lägenhetsyta : String
    , månadsavgift : String
    }


initialModel =
    { eget_kapital = "0"
    , långfristiga_skulder = "0"
    , andelstal = "0"
    , lägenhetsyta = "0"
    , månadsavgift = "0"
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = UpdateEgetKapital String
    | UpdateLångfristigaSkulder String
    | UpdateAndelstal String
    | UpdateLägenhetsyta String
    | UpdateMånadsavgift String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateEgetKapital nytt_eget_kapital ->
            ( { model | eget_kapital = nytt_eget_kapital }, Cmd.none )

        UpdateLångfristigaSkulder nytt_långfristiga_skulder ->
            ( { model | långfristiga_skulder = nytt_långfristiga_skulder }, Cmd.none )

        UpdateAndelstal nytt_andelstal ->
            ( { model | andelstal = nytt_andelstal }, Cmd.none )

        UpdateLägenhetsyta ny_lägenhetsyta ->
            ( { model | lägenhetsyta = ny_lägenhetsyta }, Cmd.none )

        UpdateMånadsavgift ny_månadsavgift ->
            ( { model | månadsavgift = ny_månadsavgift }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "Summa eget kapital:"
            , input [ onInput UpdateEgetKapital, type_ "number" ] [ text model.eget_kapital ]
            , text " kr"
            ]
        , div []
            [ text "Långfristiga skulder:"
            , input [ onInput UpdateLångfristigaSkulder, type_ "number" ] []
            , text " kr"
            ]
        , div []
            [ text "Andelstal i %:"
            , input [ onInput UpdateAndelstal, type_ "number" ] []
            , text "%"
            ]
        , div []
            [ text "Lägenhetsyta:"
            , input [ onInput UpdateLägenhetsyta, type_ "number" ] []
            , text "kvm"
            ]
        , div []
            [ text "Månadsavgift:"
            , input [ onInput UpdateMånadsavgift, type_ "number" ] []
            , text " kr/mån"
            ]
        , div []
            [ text "Belåningsgrad: "
            , text (belåningsgrad model)
            , text "%"
            ]
        , div []
            [ text "Lägenhetens del av skulden per kvadratmeter: "
            , text (skuldandel model)
            , text " kr"
            ]
        ]


belåningsgrad : Model -> String
belåningsgrad model =
    let
        kapital =
            toNumberIfPresentOrZero model.eget_kapital

        skulder =
            toNumberIfPresentOrZero model.långfristiga_skulder

        summa =
            kapital + skulder
    in
    if summa == 0 then
        ""
    else
        twoDecimal ((skulder / summa) * 100)


skuldandel : Model -> String
skuldandel model =
    let
        skulder =
            toNumberIfPresentOrZero model.långfristiga_skulder

        andel =
            toNumberIfPresentOrZero model.andelstal
    in
    if skulder == 0 || andel == 0 then
        ""
    else
        twoDecimal (skulder * andel / 100)


toNumberIfPresentOrZero : String -> Float
toNumberIfPresentOrZero string =
    Result.withDefault 0 (String.toFloat string)


twoDecimal : Float -> String
twoDecimal n =
    format { decimals = 2, thousandSeparator = " ", decimalSeparator = ",", negativePrefix = "−", negativeSuffix = "" } n



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
