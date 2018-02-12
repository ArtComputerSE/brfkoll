port module Main exposing (..)

import FormatNumber exposing (format)
import Html exposing (Html, div, input, label, table, tbody, td, text, tr)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onInput)
import Navigation
import Regex
import UrlParser exposing ((</>))


main : Program (Maybe String) Model Msg
main =
    Navigation.programWithFlags urlParser
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        }



-- Ports


port setStorage : String -> Cmd msg


port removeStorage : String -> Cmd msg



-- MODEL


type alias Model =
    { route : Route
    , parameters : Parameters
    }


type alias Parameters =
    { eget_kapital : String
    , långfristiga_skulder : String
    , andelstal : String
    , lägenhetsyta : String
    , månadsavgift : String
    }


defaultParameters : Parameters
defaultParameters =
    { eget_kapital = "0"
    , långfristiga_skulder = "0"
    , andelstal = "0"
    , lägenhetsyta = "0"
    , månadsavgift = "0"
    }


initialModel : Route -> Parameters -> Model
initialModel route parameters =
    let
        p =
            Debug.log "Parameters " (toString parameters)
    in
    { route = route
    , parameters = parameters
    }


init : Maybe String -> Navigation.Location -> ( Model, Cmd Msg )
init maybeString location =
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
    case Debug.log "Maybe stored string" maybeString of
        Just something ->
            ( initialModel route (parametersFromString something), Cmd.none )

        Nothing ->
            ( initialModel route defaultParameters, Cmd.none )


parametersFromString : String -> Parameters
parametersFromString string =
    let
        list =
            Debug.log "list" (String.split ":" string)
    in
    Parameters (pick 1 list) (pick 2 list) (pick 3 list) (pick 4 list) (pick 5 list)


pick : Int -> List String -> String
pick n list =
    if n == 1 then
        case List.head list of
            Just head ->
                head

            Nothing ->
                ""
    else
        case List.tail list of
            Just tail ->
                pick (n - 1) tail

            Nothing ->
                ""


parametersToString : Parameters -> String
parametersToString parameters =
    let
        p =
            Debug.log "Parameters to String" parameters
    in
    String.join ":"
        [ parameters.eget_kapital
        , parameters.långfristiga_skulder
        , parameters.andelstal
        , parameters.lägenhetsyta
        , parameters.månadsavgift
        ]



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


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, commands ) =
            update msg model

        newParameters =
            newModel.parameters
    in
    ( newModel
    , Cmd.batch [ commands, setStorage (parametersToString newParameters) ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        params =
            model.parameters
    in
    case msg of
        UpdateEgetKapital nytt_eget_kapital ->
            ( { model | parameters = { params | eget_kapital = nytt_eget_kapital } }, Cmd.none )

        UpdateLångfristigaSkulder nytt_långfristiga_skulder ->
            ( { model | parameters = { params | långfristiga_skulder = nytt_långfristiga_skulder } }, Cmd.none )

        UpdateAndelstal nytt_andelstal ->
            ( { model | parameters = { params | andelstal = nytt_andelstal } }, Cmd.none )

        UpdateLägenhetsyta ny_lägenhetsyta ->
            ( { model | parameters = { params | lägenhetsyta = ny_lägenhetsyta } }, Cmd.none )

        UpdateMånadsavgift ny_månadsavgift ->
            ( { model | parameters = { params | månadsavgift = ny_månadsavgift } }, Cmd.none )

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
        [ inputRow "Summa eget kapital:" UpdateEgetKapital model.parameters.eget_kapital "kr"
        , inputRow "Långfristiga skulder:" UpdateLångfristigaSkulder model.parameters.långfristiga_skulder "kr"
        , inputRow "Andelstal i %:" UpdateAndelstal model.parameters.andelstal "%"
        , inputRow "Lägenhetsyta:" UpdateLägenhetsyta model.parameters.lägenhetsyta "kvm"
        , inputRow "Månadsavgift:" UpdateMånadsavgift model.parameters.månadsavgift " kr/mån"
        , resultRow "Belåningsgrad: " (belåningsgrad model) "%"
        , resultRow "Lägenhetens del av skulden: " (skuldandel model) " kr"
        , resultRow "Lägenhetens del av skulden per kvadratmeter: " (skuldandel_per_kvm model) " kr"
        ]


inputRow label inputMessage currentValue suffix =
    tr []
        [ td [ class "col-left" ] [ text label ]
        , td [ class "col-center" ]
            [ input [ onInput inputMessage, value currentValue ] []
            ]
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
            toNumberIfPresentOrZero model.parameters.eget_kapital

        skulder =
            toNumberIfPresentOrZero model.parameters.långfristiga_skulder

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
            toNumberIfPresentOrZero model.parameters.långfristiga_skulder

        andel =
            toNumberIfPresentOrZero model.parameters.andelstal
    in
    if skulder == 0 || andel == 0 then
        ""
    else
        twoDecimal (skulder * andel / 100)


skuldandel_per_kvm : Model -> String
skuldandel_per_kvm model =
    let
        skulder =
            toNumberIfPresentOrZero model.parameters.långfristiga_skulder

        andel =
            toNumberIfPresentOrZero model.parameters.andelstal

        yta =
            toNumberIfPresentOrZero model.parameters.lägenhetsyta
    in
    if skulder == 0 || andel == 0 then
        ""
    else if yta == 0 then
        ""
    else
        twoDecimal ((skulder * andel / 100) / yta)


toNumberIfPresentOrZero : String -> Float
toNumberIfPresentOrZero string =
    Result.withDefault 0 (String.toFloat (replaceDecimalSeparator string))


replaceDecimalSeparator : String -> String
replaceDecimalSeparator string =
    Regex.replace Regex.All (Regex.regex ",") (\_ -> ".") string


twoDecimal : Float -> String
twoDecimal n =
    format { decimals = 2, thousandSeparator = " ", decimalSeparator = ",", negativePrefix = "−", negativeSuffix = "" } n



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
