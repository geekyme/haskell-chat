module Main exposing (main)

import Html exposing (Html)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import WebSocket
import Tuple exposing (mapFirst)
import Json.Decode as JD
import Json.Encode as JE
import Json.Decode.Pipeline as JDP

main : Program Flags Model Msg
main =
  Html.programWithFlags
     { init          = init
     , update        = update
     , view          = view
     , subscriptions = subscriptions
     }

type alias Model =
  { chats : List ChatMsg
  , chatBoxText: Message
  , username : Username
  , error: Maybe Error
  }

type alias Error = String
type alias Username = String
type alias Message = String
type alias ChatMsg =
  { username : Username
  , message : Message
  }

decoderChatMsg : JD.Decoder ChatMsg
decoderChatMsg =
  JDP.decode ChatMsg
    |> JDP.required "username" JD.string
    |> JDP.required "message" JD.string

type alias Flags =
  { username : Username
  }

type Msg
  = ReceiveChatMsg ChatMsg
  | TypeMsg Message
  | SendChatMsg
  | AppError Error

init : Flags -> (Model, Cmd Msg)
init { username } =
  (Model [] "" username Nothing, Cmd.none)

view : Model -> Html Msg
view model =
  div []
    [ div []
        (List.map viewMessage model.chats)
    , Html.form [ onSubmit SendChatMsg ]
        [ input [ type_ "text", onInput TypeMsg, value model.chatBoxText ] [] ]
    , viewError model
    ]

viewMessage : ChatMsg -> Html Msg
viewMessage { username, message } =
  div []
    [ text username
    , text ": "
    , text message
    ]

viewError : Model -> Html Msg
viewError model =
  case model.error of
    Just error ->
      div []
        [ text error ]

    Nothing ->
      text ""

wsUrl : String
wsUrl = "ws://localhost:3000"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SendChatMsg ->
      model
        |> sendChatMsg
        |> mapFirst (\model -> setChatBoxText "" model)

    ReceiveChatMsg chatMsg ->
      appendMsg chatMsg model ! []

    TypeMsg value ->
      setChatBoxText value model ! []

    AppError error ->
      setError (Just error) model ! []


sendChatMsg : Model -> (Model, Cmd msg)
sendChatMsg model =
  let
    value =
      JE.object
        [ ("username", JE.string model.username)
        , ("message", JE.string model.chatBoxText)
        ]

    msg = JE.encode 0 value
  in
    model ! [WebSocket.send wsUrl msg]

setChatBoxText : Message -> Model -> Model
setChatBoxText value model = { model | chatBoxText = value }

setError : Maybe Error -> Model -> Model
setError error model = { model | error = error }

appendMsg : ChatMsg -> Model -> Model
appendMsg msg model = { model | chats = model.chats ++ [msg] }

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen wsUrl (\s ->
    case JD.decodeString decoderChatMsg s of
      Ok chatMsg -> ReceiveChatMsg chatMsg
      Err err -> AppError err
    )
