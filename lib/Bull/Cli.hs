module Bull.Cli
  ( bullCli
  , BullCli(..)
  ) where

import Bull.Net
import Options.Applicative

newtype BullCli
  = DaemonCli Net
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
  ]

daemonCliParser :: Parser BullCli
daemonCliParser = DaemonCli <$> netParser

netParser :: Parser Net
netParser = asum
  [ mainnet <$> strOption (long "mainnet" <> metavar "HOST")
  , testnet <$> strOption (long "testnet" <> metavar "HOST")
  ]
