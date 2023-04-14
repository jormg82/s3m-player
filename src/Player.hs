{-# LANGUAGE RecordWildCards #-}

module Player(play) where

import Header

import qualified Data.Array as A
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Word
import System.IO


type Stream = [Word8]


play :: Header -> IO ()
play Header{..} = do
  let Just ins = M.lookup 14 instruments 
      st = B.pack $ A.elems $ buffer ins
  B.hPut stdout st
    


int2word8 :: Int -> Word8
int2word8 n = fromIntegral (floor (fromIntegral n*1.0)+0x80)
