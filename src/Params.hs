
module Params(
  Params (..),
  cmdLineParser
) where

import Options.Applicative


data Params = Params
  {
    file       :: FilePath,
    outInfo    :: Bool
  }
  deriving Show


makeParams :: Parser Params
makeParams =
  Params <$> strArgument
               (metavar "FILE" <> help "S3M file name")
         <*> switch
               (long "info" <> short 'i' <>
                help "Output file info")


cmdLineParser :: IO Params
cmdLineParser = execParser opts
  where
    opts = info (makeParams <**> helper)
                (fullDesc <> progDesc "A simple S3M player")

