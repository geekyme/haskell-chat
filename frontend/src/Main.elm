module Main exposing (main)

import Html exposing (Html)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import WebSocket
import Tuple exposing (mapFirst)

main : Program Flags Model Msg
main =
  Html.programWithFlags
     { init          = init
     , update        = update
     , view          = view
     , subscriptions = subscriptions
     }

type alias Model =
  { pokes : Int
  , chats : List ChatMsg
  , chatBoxText: Message
  , username : Username
  }

type alias Username = String
type alias Message = String
type alias ChatMsg = (Username, Message)

type alias Flags =
  { username : Username
  }

type Msg
  = Receive String
  | Send
  | ReceiveChatMsg ChatMsg
  | TypeMsg Message
  | SendChatMsg

init : Flags -> (Model, Cmd Msg)
init { username } =
  (Model 0 [] "" username, Cmd.none)

view : Model -> Html Msg
view model =
  div []
    [ p [] [ text <| "Pokes: " ++ toString model ]
    , button [ onClick Send ] [ text "Poke others" ]
    , div []
        (List.map viewMessage model.chats)
    , Html.form [ onSubmit SendChatMsg ]
        [ input [ type_ "text", onInput TypeMsg, value model.chatBoxText ] [] ]
    ]

viewMessage : ChatMsg -> Html Msg
viewMessage (username, message) =
  div []
    [ text message
    ]

wsUrl : String
wsUrl = "ws://localhost:3000"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Receive "poke" ->
      incPokes model ! []
    Receive _ ->
      model ! []
    Send ->
      model ! [ WebSocket.send wsUrl "poke" ]
    SendChatMsg ->
      model
        |> sendChatMsg
        |> mapFirst (\model -> setChatBoxText "" model)

    ReceiveChatMsg chatMsg ->
      appendMsg chatMsg model ! []
    TypeMsg value ->
      setChatBoxText value model ! []


sendChatMsg : Model -> (Model, Cmd msg)
sendChatMsg model =
  let
    msg = model.username ++ ": " ++ model.chatBoxText
  in
    model ! [WebSocket.send wsUrl msg]

setChatBoxText : Message -> Model -> Model
setChatBoxText value model = { model | chatBoxText = value }

incPokes : Model -> Model
incPokes model = { model | pokes = model.pokes + 1 }

appendMsg : ChatMsg -> Model -> Model
appendMsg msg model = { model | chats = model.chats ++ [msg] }

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocket.listen wsUrl (\s ->
    case s of
      "poke" -> Receive s
      _ -> ReceiveChatMsg ("", s)
    )
