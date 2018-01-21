module Main exposing (..)

import Html exposing (Html, text, div, h1, img, span, button)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import List exposing (map, head)
import String exposing (left)
import Tuple exposing (first, second)


---- MODEL ----


type alias Office =
    { id : Int
    , name : String
    , description : String
    , latitude : String
    , longitude : String
    , photo : Maybe String
    }


type alias Model =
    { offices : List Office
    , route : Route
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] ListView, getOffices () )



---- UPDATE ----


type alias Url =
    String


type Route
    = ListView
    | GridView
    | Map


type Msg
    = FetchOffices
    | ReceiveOffices (Result Http.Error (List Office))
    | UpdateRoute Route


router : Url -> Route
router url =
    case url of
        "list" ->
            ListView

        "grid" ->
            GridView

        "map" ->
            Map

        _ ->
            ListView


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchOffices ->
            ( model, getOffices () )

        ReceiveOffices (Ok newOffices) ->
            ( { model | offices = newOffices }, Cmd.none )

        ReceiveOffices (Err _) ->
            ( model, Cmd.none )

        UpdateRoute newRoute ->
            ( { model | route = newRoute }, Cmd.none )



---- HTTP ----


officesURL : String
officesURL =
    "https://itk-exam-api.herokuapp.com/api/offices"


getOffices : () -> Cmd Msg
getOffices () =
    let
        request =
            Http.get officesURL decodeResults
    in
        Http.send ReceiveOffices request


objectDecoder : Decode.Decoder Office
objectDecoder =
    Decode.map6
        Office
        (Decode.at [ "id" ] Decode.int)
        (Decode.at [ "name" ] Decode.string)
        (Decode.at [ "description" ] Decode.string)
        (Decode.at [ "latitude" ] Decode.string)
        (Decode.at [ "longitude" ] Decode.string)
        (Decode.at [ "photo" ] (Decode.nullable Decode.string))


decodeResults : Decode.Decoder (List Office)
decodeResults =
    (Decode.list objectDecoder)



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ header
        , renderRoute model
        ]


renderRoute : Model -> Html Msg
renderRoute model =
    case model.route of
        ListView ->
            listView model

        GridView ->
            gridView model

        Map ->
            gridView model


gridView : Model -> Html msg
gridView model =
    div [ class "grid-container" ] (map officeCard model.offices)


listView : Model -> Html msg
listView model =
    div [ class "list-container" ] (map listItem model.offices)


officeCard : Office -> Html msg
officeCard office =
    div [ class "office-card" ]
        [ getPhoto office
        , text office.name
        , text office.description
        ]


listItem : Office -> Html msg
listItem office =
    div [ class "list-item" ]
        [ getPhoto office
        , text office.name
        , text office.description
        ]


getPhoto : Office -> Html msg
getPhoto office =
    case office.photo of
        Nothing ->
            span [ class "letter-photo" ] [ text (left 1 office.name) ]

        Just url ->
            img [ src url, class "photo" ] []


headerItems : List ( String, Route )
headerItems =
    [ ( "List", ListView ), ( "Grid", GridView ), ( "Map", Map ) ]


header : Html Msg
header =
    div [ class "header" ]
        (map renderHeaderItem headerItems)


renderHeaderItem : ( String, Route ) -> Html Msg
renderHeaderItem item =
    div [ class "header-item", onClick (UpdateRoute (second item)) ] [ text (first item) ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
