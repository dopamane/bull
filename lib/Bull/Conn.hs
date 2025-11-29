-- | Network connection
module Bull.Conn
  ( Conn
  , withConn
  , sendMsg
  , recvMsg
  ) where

import Bull.Log
import Bull.Message
import Bull.Net
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Network.Run.TCP
import Network.Socket
import Network.Socket.ByteString.Lazy
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L

data Conn = Conn
  { net      :: Net
  , lgr      :: Logger
  , sendChan :: TChan Msg
  , recvChan :: TChan Msg
  , name     :: String
  }

newConn :: Net -> Logger -> Socket -> IO Conn
newConn n l s =
  Conn n l
    <$> newTChanIO
    <*> newBroadcastTChanIO
    <*> show `fmap` getPeerName s

withConn :: Net -> Logger -> (Conn -> IO a) -> IO a
withConn n l k = runTCPClient (netHost n) (netPort n) $ \sock -> do
  hndl <- newConn n l sock
  bracket_ (sayConn hndl "connected") (sayConn hndl "disconnected") $
    runConcurrently $ asum $ map Concurrently
      [ sender hndl sock
      , recver hndl sock
      , handshake hndl >> withPingPong hndl (k hndl)
      ]

sendMsg :: Conn -> Msg -> IO ()
sendMsg hndl = atomically . writeTChan (sendChan hndl)

recvMsg :: Conn -> (IO Msg -> IO a) -> IO a
recvMsg hndl k = do
  recvChan' <- atomically $ dupTChan $ recvChan hndl
  k $ atomically $ readTChan recvChan'

sender :: Conn -> Socket -> IO a
sender hndl sock = forever $ do
  msg <- atomically $ readTChan $ sendChan hndl
  sendAll sock $ runPut $ putMessage msg

recver :: Conn -> Socket -> IO a
recver hndl sock = do
  sayConn hndl "decoding messages"
  runDecoder hndl mempty $ recv sock 4096

runDecoder :: Conn -> ByteString -> IO ByteString -> IO a
runDecoder hndl bs bsIO = loop $ runGetIncremental $ getMessage $ net hndl
  where
    loop decoder
      | L.null bs = pushChunks' =<< bsIO
      | otherwise = pushChunks' bs
      where
       pushChunks' bs' = do
        case pushChunks decoder bs' of
         Fail bs'' _ _      -> runDecoder hndl (L.fromStrict bs'') bsIO
         decoder'@Partial{} -> loop decoder'
         Done bs'' _ a      -> do
           atomically $ writeTChan (recvChan hndl) a
           runDecoder hndl (L.fromStrict bs'') bsIO

withPingPong :: Conn -> IO a -> IO a
withPingPong hndl = fmap (either id id) . race (recvMsg hndl pingpong)
  where
    pingpong msgIO = do
      sayConn hndl "ping pong"
      forever $ do
        payload <- toBullPayload <$> msgIO
        case payload of
          BmpPing nonce -> sendMsg hndl $ pongMsg (net hndl) nonce
          _             -> return ()

handshake :: Conn -> IO ()
handshake hndl = recvMsg hndl $ \msgIO -> do
  sayConn hndl "starting handshake"
  sendMsg hndl =<< versionMsg (net hndl)
  versonPayload <- toBullPayload <$> msgIO
  case versonPayload of
    BmpVersion{} -> return ()
    _            -> fail "expected version"
  verackPayload <- toBullPayload <$> msgIO
  case verackPayload of
    BmpVerack -> return ()
    _         -> fail "expected verack"
  sendMsg hndl $ verackMsg $ net hndl
  sayConn hndl "handshake complete"

sayConn :: Conn -> String -> IO ()
sayConn hndl msg =
  say (lgr hndl) $ "conn " <> name hndl <> ": " <> msg
