
module Params(
  Params (..),
  cmdLineParser
) where

import Options.Applicative


data Params = Params
  {
    file       :: FilePath,
    outInfo    :: Bool,
    sampleRate :: Int
  }
  deriving Show


makeParams :: Parser Params
makeParams =
  Params <$> strArgument
               (metavar "FILE" <> help "S3M file name")
         <*> switch
               (long "info" <> short 'i' <>
                help "Output file info")
         <*> option auto
               (long "sr" <>
                help "Sample rate" <>
                showDefault <>
                value 22050 <>
                metavar "INT")


cmdLineParser :: IO Params
cmdLineParser = execParser opts
  where
    opts = info (makeParams <**> helper)
                (fullDesc <> progDesc "A simple S3M player")

