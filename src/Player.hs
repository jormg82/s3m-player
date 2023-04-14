{-# LANGUAGE RecordWildCards #-}

module Player(play) where

import qualified Header as H


import Control.Monad(replicateM)
import Control.Monad.Trans.State(evalState, State, get, put)
import qualified Data.Array as A
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Word
import System.IO


type Stream = [Word8]
type Stream2 = [(Word8, Word8)]


data ChState = ChState
  {
    samplePos :: Float 
  }
  deriving Show


data PlState = PlState
  {
    header   :: H.Header, 
    chStates :: M.Map Int ChState
  }
  deriving Show


type PS = State PlState

getHeader :: PS H.Header
getHeader = header <$> get


play :: H.Header -> IO ()
play header@H.Header{..} = do
  let initCh    = ChState{samplePos=0}
      -- OJO para el mapping poner funcion aparte
      chStates  = M.map (\_ -> ChState{samplePos=0}) channels
      initState = PlState{..}
      stream    = evalState playFile initState
  B.hPut stdout $ B.pack stream



playFile :: PS Stream
playFile = do
  H.Header{..} <- getHeader
  concat <$> traverse playPattern order


playPattern :: Int -> PS Stream
playPattern n = do
  maybePat <- (M.lookup n . H.patterns) <$> getHeader
  case maybePat of
    Just pattern -> concat <$> traverse playRow pattern
    Nothing      -> error $ "Bad pattern: " ++ show n


playRow :: H.Row -> PS Stream
-- OJO inicializar aqui tick
playRow rs = mix <$> traverse playNote rs


playNote :: H.Note -> PS Stream2
playNote note = do
  -- OJO la velocidad no sera esta
  speed <- H.initialSpeed <$> getHeader
  concat <$> replicateM speed (playTick note)


playTick :: H.Note -> PS Stream2
playTick = do
  undefined


mix :: [Stream2] -> Stream
mix = concatMap (\(w1, w2) -> [w1, w2]) . foldr1 mix2


mix2 :: Stream2 -> Stream2 -> Stream2
mix2 = zipWith $ apply2 fmix2


fmix2 :: Word8 -> Word8 -> Word8
fmix2 w1 w2 = fromIntegral $ w1'+w2'
  where
    w1' = fromIntegral w1 :: Int
    w2' = fromIntegral w2 :: Int


apply2 :: (a -> a -> b) -> (a, a) -> (a, a) -> (b, b)
apply2 f (x, y) (z, t) = (f x z, f y t)


------------------------------------------------------------
--  Para reproducir:
--  st = B.pack $ A.elems $ buffer ins
--  B.hPut stdout st
    


int2word8 :: Int -> Word8
int2word8 n = fromIntegral (floor (fromIntegral n*1.0)+0x80)
