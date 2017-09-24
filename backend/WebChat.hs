{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Concurrent as Concurrent
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets as WS
import qualified Safe

main :: IO ()
main = do
  state <- Concurrent.newMVar []
  chats <- Concurrent.newMVar []
  Warp.run 3000 $ WS.websocketsOr
    WS.defaultConnectionOptions
    (wsApp state chats)
    httpApp

httpApp :: Wai.Application
httpApp _ respond =
  respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"


type ClientId = Int
type Client = (ClientId, WS.Connection)
type State = [Client]
type Username = String
type Message = String
type ChatMsg = (Username, Message)
type Chats = [ChatMsg]

nextId :: State -> ClientId
nextId =
  Maybe.maybe 0 ((+) 1) . Safe.maximumMay . List.map fst

connectClient :: WS.Connection -> Concurrent.MVar State -> IO ClientId
connectClient conn stateRef =
  Concurrent.modifyMVar stateRef $ \state -> do
    let clientId = nextId state
    return ((clientId, conn) : state, clientId)

withoutClient :: ClientId -> State -> State
withoutClient clientId =
  List.filter ((/=) clientId . fst)

joinRoom :: ClientId -> Concurrent.MVar Chats -> IO Message
joinRoom clientId chatsRef =
  Concurrent.modifyMVar chatsRef $ \chats -> do
    let message = show clientId ++ " has joined the room."
    return (chats ++ [("system", message)], message)

leaveRoom :: ClientId -> Concurrent.MVar Chats -> IO Message
leaveRoom clientId chatsRef =
  Concurrent.modifyMVar chatsRef $ \chats -> do
    let message = show clientId ++ " has left the room."
    return (chats ++ [("system", message)], message)

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef =
  Concurrent.modifyMVar_ stateRef $ \state ->
    return $ withoutClient clientId state

listen :: WS.Connection -> ClientId -> Concurrent.MVar State -> IO ()
listen conn clientId stateRef =
  Monad.forever $ do
    msg <- WS.receiveData conn
    emit stateRef msg

broadcast :: ClientId -> Concurrent.MVar State -> Text.Text -> IO ()
broadcast clientId stateRef msg = do
  clients <- Concurrent.readMVar stateRef
  let otherClients = withoutClient clientId clients
  Monad.forM_ otherClients $ \(_, conn) ->
    WS.sendTextData conn msg

emit :: Concurrent.MVar State -> Text.Text -> IO ()
emit stateRef msg = do
  clients <- Concurrent.readMVar stateRef
  Monad.forM_ clients $ \(_, conn) ->
    WS.sendTextData conn msg

wsApp :: Concurrent.MVar State -> Concurrent.MVar Chats -> WS.ServerApp
wsApp stateRef chatsRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  message <- joinRoom clientId chatsRef
  broadcast clientId stateRef $ Text.pack message
  WS.forkPingThread conn 30
  Exception.finally
    (listen conn clientId stateRef)
    (do
      message <- leaveRoom clientId chatsRef
      broadcast clientId stateRef $ Text.pack message
      disconnectClient clientId stateRef
    )
