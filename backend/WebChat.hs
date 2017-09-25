{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Data.Aeson as Aeson (ToJSON, FromJSON, encode, decode)
import GHC.Generics
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
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Char8 (pack)

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

instance ToJSON ChatMsg
instance FromJSON ChatMsg

instance ToJSON IncomingData
instance FromJSON IncomingData

type ClientId = Int
type Client = (ClientId, WS.Connection)
type State = [Client]
type Username = String
type Message = String
type Chats = [ChatMsg]

data ChatMsg =
  ChatMsg
    { username :: Username
    , message :: Message
    } deriving Generic

data IncomingData = IncomingData
  { message :: Message
  } deriving Generic

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

joinRoom :: ClientId -> Concurrent.MVar Chats -> IO ByteString
joinRoom clientId chatsRef =
  Concurrent.modifyMVar chatsRef $ \chats -> do
    let chatMsg = ChatMsg "system" $ show clientId ++ " has joined the room."
    return (chats ++ [ chatMsg ], Aeson.encode chatMsg)

leaveRoom :: ClientId -> Concurrent.MVar Chats -> IO ByteString
leaveRoom clientId chatsRef =
  Concurrent.modifyMVar chatsRef $ \chats -> do
    let chatMsg = ChatMsg "system" $ show clientId ++ " has left the room."
    return (chats ++ [ chatMsg ], Aeson.encode chatMsg)

newMessage :: ClientId -> ChatMsg -> Concurrent.MVar Chats -> IO ByteString
newMessage clientId chatMsg chatsRef =
  Concurrent.modifyMVar chatsRef $ \chats -> do
    return (chats ++ [ chatMsg ], Aeson.encode chatMsg)

loadMessages :: WS.Connection -> ClientId -> Concurrent.MVar Chats -> IO ()
loadMessages conn clientId chatsRef = do
  chats <- Concurrent.readMVar chatsRef
  Monad.forM_ chats $ \chatMsg ->
    WS.sendTextData conn $ Aeson.encode chatMsg

disconnectClient :: ClientId -> Concurrent.MVar State -> IO ()
disconnectClient clientId stateRef =
  Concurrent.modifyMVar_ stateRef $ \state ->
    return $ withoutClient clientId state

listen :: WS.Connection -> ClientId -> Concurrent.MVar State -> Concurrent.MVar Chats -> IO ()
listen conn clientId stateRef chatsRef =
  Monad.forever $ do
    bytestring <- WS.receiveData conn
    let m =  Aeson.decode bytestring :: Maybe IncomingData
    case m of
      Nothing -> do
        return ()
      Just dat -> do
        let chatMsg = ChatMsg (show clientId) (message (dat :: IncomingData))
        messageWithUser <- newMessage clientId chatMsg chatsRef
        emit stateRef messageWithUser
        return ()

broadcast :: ClientId -> Concurrent.MVar State -> ByteString -> IO ()
broadcast clientId stateRef msg = do
  clients <- Concurrent.readMVar stateRef
  let otherClients = withoutClient clientId clients
  Monad.forM_ otherClients $ \(_, conn) ->
    WS.sendTextData conn msg

emit :: Concurrent.MVar State -> ByteString -> IO ()
emit stateRef msg = do
  clients <- Concurrent.readMVar stateRef
  Monad.forM_ clients $ \(_, conn) ->
    WS.sendTextData conn msg

wsApp :: Concurrent.MVar State -> Concurrent.MVar Chats -> WS.ServerApp
wsApp stateRef chatsRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  bytestring <- joinRoom clientId chatsRef
  broadcast clientId stateRef bytestring
  loadMessages conn clientId chatsRef
  WS.forkPingThread conn 30
  Exception.finally
    (listen conn clientId stateRef chatsRef)
    (do
      bytestring <- leaveRoom clientId chatsRef
      broadcast clientId stateRef bytestring
      disconnectClient clientId stateRef
    )
