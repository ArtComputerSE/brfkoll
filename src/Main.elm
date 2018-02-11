module Main exposing (..)

import FormatNumber exposing (format)
import Html exposing (Html, div, input, label, table, tbody, td, text, tr)
import Html.Attributes exposing (class, type_)
import Html.Events exposing (onInput)
import Navigation
import UrlParser exposing ((</>))


main : Program Never Model Msg
main =
    Navigation.program urlParser
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { route : Route
    , eget_kapital : String
    , långfristiga_skulder : String
    , andelstal : String
    , lägenhetsyta : String
    , månadsavgift : String
    }


initialModel : Route -> Model
initialModel route =
    { route = route
    , eget_kapital = "0"
    , långfristiga_skulder = "0"
    , andelstal = "0"
    , lägenhetsyta = "0"
    , månadsavgift = "0"
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        l =
            Debug.log "init location" location

        route =
            case UrlParser.parsePath routeParser location of
                Nothing ->
                    HomeRoute

                Just route ->
                    route
    in
    ( initialModel route, Cmd.none )



-- ROUTES


type alias CodedBrfRecord =
    String


type Route
    = HomeRoute
    | AddBrfRoute CodedBrfRecord
    | NotFound



-- UPDATE


type Msg
    = UpdateEgetKapital String
    | UpdateLångfristigaSkulder String
    | UpdateAndelstal String
    | UpdateLägenhetsyta String
    | UpdateMånadsavgift String
    | FollowRoute Route


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

        FollowRoute route ->
            ( { model | route = route }, Cmd.none )



-- PARSING


urlParser : Navigation.Location -> Msg
urlParser location =
    let
        l =
            Debug.log "location" location

        parsed =
            UrlParser.parsePath routeParser location
    in
    case Debug.log "parsed" parsed of
        Nothing ->
            FollowRoute NotFound

        Just route ->
            FollowRoute route


postsParser : UrlParser.Parser a a
postsParser =
    UrlParser.s "posts"


addBrfParser : UrlParser.Parser (CodedBrfRecord -> a) a
addBrfParser =
    UrlParser.s "add" </> UrlParser.string


homeParser : UrlParser.Parser a a
homeParser =
    UrlParser.oneOf
        [ UrlParser.s "index.html"
        , UrlParser.s ""
        ]


routeParser : UrlParser.Parser (Route -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map AddBrfRoute addBrfParser
        , UrlParser.map HomeRoute homeParser
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "overflow-container" ]
        [ case model.route of
            HomeRoute ->
                viewCalculator model

            AddBrfRoute s ->
                notImplementedYetPage model s

            NotFound ->
                notFoundPage model
        ]


viewCalculator : Model -> Html Msg
viewCalculator model =
    table []
        [ inputRow "Summa eget kapital:" UpdateEgetKapital model.eget_kapital "kr"
        , inputRow "Långfristiga skulder:" UpdateLångfristigaSkulder model.långfristiga_skulder "kr"
        , inputRow "Andelstal i %:" UpdateAndelstal model.andelstal "%"
        , inputRow "Lägenhetsyta:" UpdateLägenhetsyta model.lägenhetsyta "kvm"
        , inputRow "Månadsavgift:" UpdateMånadsavgift model.månadsavgift " kr/mån"
        , resultRow "Belåningsgrad: " (belåningsgrad model) "%"
        , resultRow "Lägenhetens del av skulden: " (skuldandel model) " kr"
        , resultRow "Lägenhetens del av skulden per kvadratmeter: " (skuldandel_per_kvm model) " kr"
        ]


inputRow label inputMessage currentValue suffix =
    tr []
        [ td [ class "col-left" ] [ text label ]
        , td [ class "col-center" ] [ input [ onInput inputMessage, type_ "number" ] [ text currentValue ] ]
        , td [ class "col-right" ] [ text suffix ]
        ]


resultRow label result suffix =
    tr []
        [ td [ class "col-left" ] [ text label ]
        , td [ class "col-center" ] [ text result ]
        , td [ class "col-right" ] [ text suffix ]
        ]


notImplementedYetPage : Model -> String -> Html Msg
notImplementedYetPage model code =
    div [] [ text ("This has not been implemented yet. Code: " ++ code) ]


notFoundPage : Model -> Html Msg
notFoundPage model =
    div [] [ text "Not found" ]



-- Calculations


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


skuldandel_per_kvm : Model -> String
skuldandel_per_kvm model =
    let
        skulder =
            toNumberIfPresentOrZero model.långfristiga_skulder

        andel =
            toNumberIfPresentOrZero model.andelstal

        yta =
            toNumberIfPresentOrZero model.lägenhetsyta
    in
    if skulder == 0 || andel == 0 then
        ""
    else if yta == 0 then
        ""
    else
        twoDecimal ((skulder * andel / 100) / yta)


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
