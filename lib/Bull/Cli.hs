module Bull.Cli
  ( bullCli
  , BullCli(..)
  ) where

import Bull.Net
import Bull.Server
import Options.Applicative

data BullCli
  = DaemonCli
  | ClientCli String String Rpc
  deriving (Eq, Read, Show)

bullCli :: IO BullCli
bullCli = customExecParser prefs' $
  info (bullCliParser <**> helper) $ mconcat
    [ header "Bitcoin Bull!"
    , footer "(c) David Cox 2025"
    ]
  where
    prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

bullCliParser :: Parser BullCli
bullCliParser = hsubparser $ mconcat
  [ command "daemon" $ info daemonCliParser mempty
  , command "client" $ info clientCliParser mempty
  ]

daemonCliParser :: Parser BullCli
daemonCliParser = pure DaemonCli

netParser :: Parser Net
netParser = asum
  [ mainnet <$> strOption (long "mainnet" <> metavar "HOST")
  , testnet <$> strOption (long "testnet" <> metavar "HOST")
  ]

clientCliParser :: Parser BullCli
clientCliParser =
  ClientCli
    <$> hostParser
    <*> portParser
    <*> rpcParser

hostParser :: Parser String
hostParser = strOption $ long "host" <> value "127.0.0.1"

portParser :: Parser String
portParser = strOption $ long "port" <> short 'p' <> value "8000"

rpcParser :: Parser Rpc
rpcParser = hsubparser $ mconcat
  [ command "connect"    $ info connectParser mempty
  , command "disconnect" $ info disconnectParser mempty
  ]

connectParser :: Parser Rpc
connectParser = Connect <$> netParser

disconnectParser :: Parser Rpc
disconnectParser = Disconnect <$> netParser
