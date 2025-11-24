module Bull.Cli
  ( bullCli
  , BullCli(..)
  ) where

import Bull.Net
import Options.Applicative

data BullCli
  = BullClientCli BullNet
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
  [ command "client" $ info bullClientCliParser mempty
  ]

bullClientCliParser :: Parser BullCli
bullClientCliParser = BullClientCli <$> netParser

netParser :: Parser BullNet
netParser = asum
  [ mainnet <$> strOption (long "mainnet" <> metavar "HOST")
  , testnet <$> strOption (long "testnet" <> metavar "HOST")
  ]
