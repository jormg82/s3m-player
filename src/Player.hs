{-# LANGUAGE RecordWildCards #-}

module Player(playFile) where

import qualified Header as H


import Control.Monad(replicateM_)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.State(evalStateT, StateT, get, put)
import qualified Data.Array as A
import qualified Data.ByteString as B
import Data.Foldable(traverse_)
import qualified Data.Map as M
import Data.Word
import System.IO


type Stream = [Word8]
type Stream2 = [(Word8, Word8)]

-- Constantes
zero :: Word8
zero = 0x80

sampleRate:: Int
sampleRate = 8363

baseFreq :: Int
baseFreq = 428

incNumerator :: Float
incNumerator = fromIntegral $ sampleRate * baseFreq


data ChState = ChState
  {
    instrument :: Maybe Int,
    samplePos  :: Float,
    frequence  :: Int
  }
  deriving Show


data PlState = PlState
  {
    header         :: H.Header, 
    speed          :: Int,
    bpm            :: Int,
    hz             :: Int,
    samplesPerTick :: Int,
    chStates       :: M.Map Int ChState
  }
  deriving Show


initChState :: ChState
initChState = ChState
  {
    samplePos  = 0,
    instrument = Nothing,
    frequence  = 0
  }


type PS = StateT PlState IO


getHeader :: PS H.Header
getHeader = header <$> get

getSpeed :: PS Int
getSpeed = speed <$> get

getBpm :: PS Int
getBpm = bpm <$> get

getHz :: PS Int
getHz = hz <$> get

getSamplesPerTick :: PS Int
getSamplesPerTick = samplesPerTick <$> get

getChannelState :: Int -> PS ChState
getChannelState n = do
  chstates <- chStates <$> get
  case M.lookup n chstates of
    Just ch -> return ch
    Nothing -> error "Channel do not exist!"


putSpeed :: Int -> PS ()
putSpeed n = get >>= \s -> put s{speed=n}

-- Cambian tambien hz y samplesPerTick
putBpm :: Int -> PS ()
putBpm n = do
  s <- get
  let hz=calcHz n
      samplesPerTick=calcSamplesPerTick hz
  put s{bpm=n,
        hz=hz,
        samplesPerTick=samplesPerTick}

putChState :: Int -> ChState -> PS ()
putChState nchan chState = do
  state@PlState{..} <- get
  put state{chStates=M.insert nchan chState chStates}


playFile :: H.Header -> IO ()
playFile header = do
  let speed=H.initialSpeed header
      bpm=H.initialTempo header
      hz=calcHz bpm
      samplesPerTick=calcSamplesPerTick hz 
      chStates=M.map (const initChState) (H.channels header)

  evalStateT play PlState{..}


play :: PS ()
play = do
  patterns <- H.order <$> getHeader
  traverse_ playPattern patterns


playPattern :: Int -> PS ()
playPattern n = do
  maybePat <- (M.lookup n . H.patterns) <$> getHeader
  case maybePat of
    Just rows -> traverse_ playRow rows
    Nothing   -> error $ "Bad pattern: " ++ show n


playRow :: H.Row -> PS ()
playRow notes = do
  initRow notes
  speed <- getSpeed
  replicateM_ speed playTick


initRow :: H.Row -> PS ()
initRow = do
  -- OJO completar inicializacion de info de canales
  return ()


playTick :: PS ()
playTick = do
  channels <- (M.keys . H.channels) <$> getHeader
  -- generamos la minima mezcla posible
  stream <- mix <$> traverse playChTick channels
  liftIO $ B.hPut stdout $ B.pack stream


playChTick :: Int -> PS Stream2
playChTick nchan = do
  spt <- getSamplesPerTick
  chstate@ChState{..} <- getChannelState nchan
  H.Header{..} <- getHeader
  let maybeins = instrument >>= flip M.lookup instruments
      (pos, samples) = generateSamples spt samplePos frequence maybeins
  putChState nchan chstate{samplePos=pos}
  return samples
  


generateSamples :: Int                -- Samples per tick
                -> Float              -- Sample pos
                -> Int                -- Frequence
                -> Maybe H.Instrument -- Instrument
                -> (Float, Stream2)   -- (New pos, stream)
generateSamples spt pos _ Nothing = (pos, replicate spt (zero, zero))
generateSamples spt pos freq (Just H.Instrument{..}) =
  (nextpos, map (dupe . (buffer A.!)) positions)
  where
    begin     = fromIntegral loopBegin
    end       = fromIntegral loopEnd + 1
    len       = end - begin + 1
    inc       = incNumerator / fromIntegral (freq*c2spd)
    nextpos   = retarg $ pos+fromIntegral spt*inc
    positions = map (floor . retarg) [pos, pos+inc..]

    retarg :: Float -> Float
    retarg pos
      | pos < end+1 = pos
      | otherwise = begin + modFloat (pos-end) len



mix :: [Stream2] -> Stream
mix = concatMap (\(w1, w2) -> [w1, w2]) . foldr1 mix2


mix2 :: Stream2 -> Stream2 -> Stream2
mix2 = zipWith $ apply2 fmix


fmix :: Word8 -> Word8 -> Word8
fmix w1 w2 = fromIntegral $ w1'+w2'
  where
    w1' = fromIntegral w1 :: Int
    w2' = fromIntegral w2 :: Int

-- Calcula los ticks por segundo a partir del bpm
calcHz :: Int -> Int
calcHz bpm = (2 * bpm) `div` 5


-- Calcula los samples por tick a partir del sample rate
-- y de los hz
calcSamplesPerTick :: Int -> Int
calcSamplesPerTick hz = sampleRate `div` hz


-- Utils
apply2 :: (a -> a -> b) -> (a, a) -> (a, a) -> (b, b)
apply2 f (x, y) (z, t) = (f x z, f y t)

dupe :: a -> (a, a)
dupe x = (x, x)

modFloat :: Float -> Float -> Float
modFloat x y = y * snd (properFraction $ x/y)

