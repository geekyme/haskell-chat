{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Data.Aeson as Aeson (ToJSON, FromJSON, encode, decode, eitherDecode)
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
import qualified Data.Map as Map

main :: IO ()
main = do
  state <- Concurrent.newMVar []
  chats <- Concurrent.newMVar []
  usernames <- Concurrent.newMVar Map.empty
  Warp.run 3000 $ WS.websocketsOr
    WS.defaultConnectionOptions
    (wsApp state chats usernames)
    httpApp

httpApp :: Wai.Application
httpApp _ respond =
  respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"

instance ToJSON ChatMsg
instance FromJSON ChatMsg

instance FromJSON ChatMsgData
instance FromJSON SetUsernameData
instance FromJSON IncomingData

type ClientId = Int
type Client = (ClientId, WS.Connection)
type State = [Client]
type Username = String
type Message = String
type Chats = [ChatMsg]
type Usernames = Map.Map ClientId Username
data ChatMsg =
  ChatMsg
    { username :: Username
    , message :: Message
    } deriving Generic

data ChatMsgData = ChatMsgData
  { message :: Message
  } deriving Generic

data SetUsernameData = SetUsernameData
  { username :: Username
  } deriving (Generic, Show)

data IncomingData
  = ChatMsgData_ ChatMsgData
  | SetUsernameData_ SetUsernameData
  deriving Generic

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

joinRoom :: ClientId -> Concurrent.MVar Usernames -> Concurrent.MVar Chats -> IO ByteString
joinRoom clientId usernamesRef chatsRef = do
  un <- getUsername clientId usernamesRef
  Concurrent.modifyMVar chatsRef $ \chats -> do
    let chatMsg = ChatMsg "system" $ un ++ " has joined the room."
    return (chats ++ [ chatMsg ], Aeson.encode chatMsg)

leaveRoom :: ClientId -> Concurrent.MVar Usernames -> Concurrent.MVar Chats -> IO ByteString
leaveRoom clientId usernamesRef chatsRef = do
  un <- getUsername clientId usernamesRef
  Concurrent.modifyMVar chatsRef $ \chats -> do
    let chatMsg = ChatMsg "system" $ un ++ " has left the room."
    return (chats ++ [ chatMsg ], Aeson.encode chatMsg)

newMessage :: Username -> Message -> Concurrent.MVar Chats -> IO ByteString
newMessage un msg chatsRef =
  Concurrent.modifyMVar chatsRef $ \chats -> do
    let chatMsg = ChatMsg un msg
    return (chats ++ [ chatMsg ], Aeson.encode chatMsg)

loadMessages :: WS.Connection -> ClientId -> Concurrent.MVar Chats -> IO ()
loadMessages conn clientId chatsRef = do
  chats <- Concurrent.readMVar chatsRef
  Monad.forM_ chats $ \chatMsg ->
    WS.sendTextData conn $ Aeson.encode chatMsg

disconnectClient :: ClientId -> Concurrent.MVar State -> Concurrent.MVar Usernames -> IO ()
disconnectClient clientId stateRef usernamesRef = do
  Concurrent.modifyMVar_ stateRef $ \state ->
    return $ withoutClient clientId state

  Concurrent.modifyMVar_ usernamesRef $ \usernames ->
    return $ Map.delete clientId usernames

listen :: WS.Connection -> ClientId -> Concurrent.MVar State -> Concurrent.MVar Chats -> Concurrent.MVar Usernames -> IO ()
listen conn clientId stateRef chatsRef usernamesRef =
  Monad.forever $ do
    bytestring <- WS.receiveData conn
    let e =  Aeson.eitherDecode bytestring :: Either String IncomingData
    case e of
      Left e -> do
        putStrLn e
        return ()
      Right incomingData -> do
        case incomingData of
          ChatMsgData_ dat -> do
            let msg = message (dat :: ChatMsgData)
            un <- getUsername clientId usernamesRef
            messageWithUser <- newMessage un msg chatsRef
            emit stateRef messageWithUser
            return ()
          SetUsernameData_ dat -> do
            let un = username (dat :: SetUsernameData)
            setUsername clientId un usernamesRef
            emit stateRef $ Aeson.encode $ ChatMsg "system" $ show clientId ++ " has set username to " ++ un
            return ()

getUsername :: ClientId -> Concurrent.MVar Usernames -> IO String
getUsername clientId usernamesRef = do
  usernames <- Concurrent.readMVar usernamesRef
  let defaultUsername = show clientId
  return $ Map.findWithDefault defaultUsername clientId usernames

setUsername :: ClientId -> Username -> Concurrent.MVar Usernames -> IO ()
setUsername clientId un usernamesRef =
  Concurrent.modifyMVar_ usernamesRef $ \usernames ->
    return $ Map.insert clientId un usernames

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

wsApp :: Concurrent.MVar State -> Concurrent.MVar Chats -> Concurrent.MVar Usernames -> WS.ServerApp
wsApp stateRef chatsRef usernamesRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  bytestring <- joinRoom clientId usernamesRef chatsRef
  broadcast clientId stateRef bytestring
  loadMessages conn clientId chatsRef
  WS.forkPingThread conn 30
  Exception.finally
    (listen conn clientId stateRef chatsRef usernamesRef)
    (do
      bytestring <- leaveRoom clientId usernamesRef chatsRef
      broadcast clientId stateRef bytestring
      disconnectClient clientId stateRef usernamesRef
    )
