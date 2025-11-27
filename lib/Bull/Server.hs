{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Bull.Server
  ( Server
  , withServer
  , sendServer
  , recvServer
  , Rpc(..)
  ) where

import Bull.Log
import Bull.Message
import Bull.Net
import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import GHC.Generics
import Network.Run.TCP
import Network.Socket
import Network.Socket.ByteString.Lazy
import Prettyprinter

data Server = Server
  { lgr :: Logger
  , sendChan :: TChan Rpc
  , recvChan :: TChan Rpc
  }

newServer :: Logger -> IO Server
newServer l = Server l <$> newTChanIO <*> newBroadcastTChanIO

withServer
  :: String -- ^ host
  -> String -- ^ port
  -> Logger
  -> (Server -> IO a)
  -> IO a
withServer host port l k = runTCPServer (Just host) port $ \sock -> do
  srvr <- newServer l
  bracket_ (sayConn srvr sock) (sayDisconn srvr sock) $
    runConcurrently $ asum $ map Concurrently
      [ sender srvr sock
      , recver srvr sock
      , k srvr
      ]

sayConn :: Server -> Socket -> IO ()
sayConn srvr = sayServer srvr . ("connected " <>) . show <=< getPeerName

sayDisconn :: Server -> Socket -> IO ()
sayDisconn srvr = sayServer srvr . ("disconnected " <>) . show <=< getPeerName

sayServer :: Server -> String -> IO ()
sayServer srvr = say (lgr srvr) . ("server: " <>)

sendServer :: Server -> Rpc -> IO ()
sendServer srvr = atomically . writeTChan (sendChan srvr)

recvServer :: Server -> (IO Rpc -> IO a) -> IO a
recvServer srvr k = do
  recvChan' <- atomically $ dupTChan $ recvChan srvr
  k $ atomically $ readTChan recvChan'

sender :: Server -> Socket -> IO a
sender srvr sock = forever $ do
  rpc <- atomically $ readTChan $ sendChan srvr
  sendAll sock $ encode rpc

recver :: Server -> Socket -> IO a
recver srvr sock = do
  say (lgr srvr) "decoding rpcs"
  runDecoder srvr mempty $ recv sock 4096

runDecoder :: Server -> ByteString -> IO ByteString -> IO a
runDecoder hndl bs bsIO = loop $ runGetIncremental get
  where
    loop decoder
      | L.null bs = pushChunks' =<< bsIO
      | otherwise = pushChunks' bs
      where
       pushChunks' bs'
         | L.null bs' = fail "recv null"
         | otherwise  = do
           case pushChunks decoder bs' of
             Fail{}             -> fail "decoding failed"
             decoder'@Partial{} -> loop decoder'
             Done bs'' _ a      -> do
               atomically $ writeTChan (recvChan hndl) a
               runDecoder hndl (L.fromStrict bs'') bsIO

data Rpc
  = Connect    Net
  | Disconnect Net
  | Listen     Net
  | Message    Msg
  | Nets       [Net]
  deriving (Binary, Eq, Generic, Read, Show)

instance Pretty Rpc where
  pretty rpc = case rpc of
    Connect    n -> vsep [pretty "connect:", indent 2 $ pretty n]
    Disconnect n -> vsep [pretty "disconnect:", indent 2 $ pretty n]
    Listen     n -> vsep [pretty "listen:", indent 2 $ pretty n]
    Message    m -> vsep [pretty m, indent 4 $ pretty $ toBullPayload m]
    Nets      ns -> vsep $ pretty <$> ns
