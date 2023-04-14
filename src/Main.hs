
module Main where

import Header
import HeaderReport
import Loader(loadFile)
import qualified Params as P
import Player

import qualified Data.Text.IO as TIO


main :: IO ()
main = do
  params <- P.cmdLineParser
  header <- loadFile $ P.file params
  if P.outInfo params then
    TIO.putStr $ headerReport header
    --print header
  else
    play header

