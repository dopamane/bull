module Bull.Cli
  ( bullCli
  , BullCli(..)
  ) where

import Options.Applicative

data BullCli
  = BullClientCli String String
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
bullClientCliParser =
  BullClientCli
    <$> hostParser
    <*> portParser

hostParser :: Parser String
hostParser = strOption $ long "host"

portParser :: Parser String
portParser = asum
  [ strOption     $ long "port"
  , flag' "8333"  $ long "mainnet"
  , flag' "18333" $ long "testnet"
  ]
