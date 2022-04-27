module ProgramOptions where

import Prelude
import Types
import Options.Applicative

data ProgramOptions = ProgramOptions
  {
    dbname :: DbName,
    user :: UserId,
    password :: Password,
    authCode :: AuthCode
  }

cmdArgsParser :: Parser ProgramOptions
cmdArgsParser =
  ProgramOptions
  <$> parsedbname
  <*> parseuser
  <*> parsepassword
  <*> parseauthCode
  where
    parsedbname =
      strOption $
        long "dbname" <> metavar "DATABASE NAME"
    parseuser =
      strOption $
        long "user" <> metavar "USER NAME"
    parsepassword =
      strOption $
        long "password" <> metavar "DATABASE PASSWORD"
    parseauthCode =
      strOption $
        long "authcode" <> metavar "IMAGGA API AUTH KEY"
