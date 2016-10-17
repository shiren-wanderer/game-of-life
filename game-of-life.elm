module Main exposing (..)

import Html exposing (div, span, text, button, input)
import Html.Attributes as Attr exposing (style, type', min, max, value)
import Html.App exposing (program)
import Html.Events exposing (onClick, onInput)
import Matrix exposing (..)
import Matrix.Random exposing (..)
import Time exposing (Time, millisecond)
import Random exposing (..)
import String


type alias Model =
    { size : Int
    , field : Matrix Bool
    , running : Bool
    , speed : Float
    }


type Msg
    = Update Time
    | UpdateSpeed String
    | Next
    | OnOff
    | Reset
    | NewField Int (Matrix Bool)


main : Program Never
main =
    program
        { init = init initSize initSpeed
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Int -> Float -> ( Model, Cmd Msg )
init size speed =
    Model 0 (Matrix.matrix size size (\loc -> False)) False speed ! [ Random.generate (NewField size) (randomField size) ]


initSpeed : Float
initSpeed =
    500


initSize : Int
initSize =
    50


randomField : Int -> Generator (Matrix Bool)
randomField size =
    Matrix.Random.matrix (Random.int size size) (Random.int size size) (Random.bool)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Next ->
            { model | field = nextGenerationMatrix model, running = False } ! []

        Update x ->
            { model | field = nextGenerationMatrix model } ! []

        UpdateSpeed newSpeedStr ->
            let
                intConvResult =
                    String.toFloat newSpeedStr
            in
                case intConvResult of
                    Result.Ok newSpeed ->
                        { model | speed = newSpeed } ! []

                    Result.Err err ->
                        { model | speed = initSpeed } ! []

        NewField size field ->
            Model size field False model.speed ! []

        OnOff ->
            { model | running = not model.running } ! []

        Reset ->
            init model.size model.speed


nextGenerationMatrix : Model -> Matrix Bool
nextGenerationMatrix model =
    Matrix.mapWithLocation (\loc -> nextGenerationAt model.field loc) model.field


nextGenerationAt : Matrix Bool -> Location -> Bool -> Bool
nextGenerationAt oldField loc element =
    let
        num =
            numOfSurround loc oldField
    in
        if num > 3 || num < 2 then
            False
        else if (num == 2 || num == 3) && element then
            True
        else if num == 3 && not element then
            True
        else
            False


numOfSurround : Location -> Matrix Bool -> Int
numOfSurround loc field =
    surroundLocations loc
        |> List.map (numAt field)
        |> List.sum


numAt : Matrix Bool -> Location -> Int
numAt field loc =
    case Matrix.get loc field of
        Just v ->
            if v then
                1
            else
                0

        Nothing ->
            0


surroundLocations : Location -> List Location
surroundLocations ( x, y ) =
    [ ( x - 1, y - 1 )
    , ( x - 1, y )
    , ( x - 1, y + 1 )
    , ( x, y - 1 )
    , ( x, y + 1 )
    , ( x + 1, y - 1 )
    , ( x + 1, y )
    , ( x + 1, y + 1 )
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.running then
        Time.every (model.speed * millisecond) Update
    else
        Sub.none


view : Model -> Html.Html Msg
view model =
    div []
        [ div []
            [ button [ style buttonStyle, onClick Next ] [ text "Next" ]
            , button [ style buttonStyle, onClick OnOff ] [ text <| onOffButtonText model.running ]
            , button [ style buttonStyle, onClick Reset ] [ text "Reset" ]
            , input
                [ type' "range"
                , Attr.min "100"
                , Attr.max "1000"
                , Attr.step "100"
                , value <| toString model.speed
                , onInput UpdateSpeed
                ]
                []
            , span [] [ text <| (toString <| model.speed / 1000) ++ "sec" ]
            ]
        , div [] <| List.map printRow <| Matrix.toList model.field
        ]


onOffButtonText : Bool -> String
onOffButtonText running =
    if running then
        "Stop"
    else
        "Run"


printRow : List Bool -> Html.Html Msg
printRow list =
    div [ style [ ( "height", "18px" ) ] ] <|
        List.map
            (\b ->
                span []
                    [ text <|
                        (if b then
                            "⬛"
                         else
                            "⬜"
                        )
                    ]
            )
            list


buttonStyle : List ( String, String )
buttonStyle =
    [ ( "height", "50px" )
    , ( "width", "100px" )
    , ( "font-size", "2em" )
    ]
