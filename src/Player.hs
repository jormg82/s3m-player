{-# LANGUAGE RecordWildCards #-}

module Player(playFile, interpolate, retarg) where

import qualified Header as H


import Control.Monad(filterM, replicateM_)
import Control.Monad.IO.Class(liftIO)
import Control.Monad.Trans.State(evalStateT, StateT, get, put)
import qualified Data.Array as A
import qualified Data.Bits as BT
import qualified Data.ByteString as B
import Data.Foldable(traverse_)
import qualified Data.Map as M
import Data.Int
import Data.Word
import System.IO


type Stream = [Int16]
type Stream2 = [(Int16, Int16)]


-- Constantes
zero :: Int16
zero = 0

zero2 :: (Int16, Int16)
zero2 = (0, 0)

sampleRate:: Int
sampleRate = 11025



-- Datos
data ChState = ChState
  {
    instrument :: Maybe Int,
    samplePos  :: Float,
    frequence  :: Int,
    volume     :: Maybe Int,
    pan        :: Int
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
    frequence  = 0,
    volume     = Nothing,
    pan        = 7
  }


type PS = StateT PlState IO


-- State utilities
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


channelEnabled :: Int -> PS Bool
channelEnabled n = do
  chstates <- chStates <$> get
  case M.lookup n chstates of
    Just ch -> return True
    Nothing -> return False


getChannelState :: Int -> PS ChState
getChannelState n = do
  chstates <- chStates <$> get
  case M.lookup n chstates of
    Just ch -> return ch
    Nothing -> error $ "Channel do not exist!: " ++ show n


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


-- main functions
playFile :: H.Header -> IO ()
playFile header = do
  let speed=H.initialSpeed header
      bpm=H.initialTempo header
      hz=calcHz bpm
      samplesPerTick=calcSamplesPerTick hz 

      initChFn H.Channel{..} =
        initChState{pan=if H.defaultPanning header then defaultPan else 7}
      chStates=M.map initChFn $ H.channels header

  evalStateT play PlState{..}


play :: PS ()
play = do
  patterns <- H.order <$> getHeader
  traverse_ playPattern patterns


playPattern :: Int -> PS ()
playPattern pat = do
  maybePat <- (M.lookup pat . H.patterns) <$> getHeader
  case maybePat of
    Just rows -> traverse_ playRow rows
    Nothing   -> error $ "Bad pattern: " ++ show pat


playRow :: H.Row -> PS ()
playRow notes = do
  validNotes <- filterM (channelEnabled . H.channel) notes
  traverse_ updateChState validNotes

  traverse_ updateNoteEffect validNotes

  speed <- getSpeed
  --liftIO $ putStrLn $ "speed vale: " ++ show speed
  replicateM_ speed playTick


updateChState :: H.Note -> PS ()
updateChState H.Note{..} = do
  -- OJO completar inicializacion de info de canales
  chstate <- getChannelState channel
  let chstate'  = case instrument of
                    Nothing -> chstate
                    Just i -> chstate{instrument=Just i, samplePos=0}
      chstate'' = case note of
                    Nothing -> chstate'
                    Just n -> chstate'{frequence=note2freq n, volume=noteVol}

  putChState channel chstate''



updateNoteEffect :: H.Note -> PS () 
updateNoteEffect H.Note{..} = do
  chstate <- getChannelState channel

  case (effect, effectParam) of

    -- Axx (Set Speed)
    (Just 1, Just speed) -> do
      putSpeed speed

    -- Oxy (Sample offset)
    (Just 15, Just o) -> do
      putChState channel chstate{samplePos=fromIntegral (o*0x100)}

    -- SX- effects
    (Just 19, Just o) -> do
      let up = BT.shift o (-4)
          down = o BT..&. 0x0F

      case up of
 
        -- S8x (Pan position)
        8 -> do
          putChState channel chstate{pan=down}

        -- default case
        _ -> do
          return ()

    -- Txx (Set Tempo)
    (Just 20, Just tempo) -> do
      putBpm tempo

    -- default case (no implementado o no existente)
    _ -> return ()



playTick :: PS ()
playTick = do
  channels <- (M.keys . H.channels) <$> getHeader
  -- generamos la minima mezcla posible
  stream <- mix <$> traverse playChTick channels
  -- OJO reponer
  liftIO $ B.hPut stdout $ B.pack stream
  return ()


playChTick :: Int -> PS Stream2
playChTick nchan = do
  spt <- getSamplesPerTick
  chstate@ChState{..} <- getChannelState nchan
  H.Header{..} <- getHeader

  let maybeins = instrument >>= flip M.lookup instruments
  case maybeins of
    Nothing -> return $ replicate spt zero2
    Just H.Instrument{..} -> do
      let begin = fromIntegral loopBegin
          end = fromIntegral loopEnd
          len = end - begin
          -- formula empirica
          inc = fromIntegral c2spd * 1712 /
                fromIntegral (sampleRate * frequence)
          positions = take spt $
            map (retarg looped begin end) [samplePos, samplePos+inc..]
          nextpos = retarg looped begin end $ samplePos+fromIntegral spt*inc
          samples = map (interpolate buffer sampleLength) positions
         
          -- aplicamos volumen
          volVal = case volume of {Nothing -> 64; Just v  -> v}
          funVol :: Int16 -> Int
          funVol s =
            (fromIntegral s * globalVol * insVol * volVal) `div` (64*64*64)
          samples' = map (fromIntegral . funVol) samples 

          -- aplicamos panning
          samples'' = map (applyPan pan) samples'
 
      putChState nchan chstate{samplePos=nextpos}       
      return samples''


interpolate :: A.Array Int Int16 -- Buffer
            -> Int               -- Buffer length
            -> Float             -- position (0<=, <buffer length)
            -> Int16
interpolate buffer len pos =
  if pos > fromIntegral len then zero
  else floor $ w1 + (w2-w1) * snd (properFraction pos)
  where
    b1 = floor pos `mod` len
    b2 = ceiling pos `mod` len 
    w1 = fromIntegral $ buffer A.! b1
    w2 = fromIntegral $ buffer A.! b2

retarg :: Bool -> Float -> Float -> Float -> Float
retarg looped begin end pos
  | pos < end || not looped = pos
  | otherwise = begin+modFloat (pos-begin) (end-begin)


mix :: [Stream2] -> [Word8]
mix = map H.int162word8
    . concatMap (\(w1, w2) -> [w1, w2])
    . foldr1 (zipWith $ apply2 (+))


-- Calcula los ticks por segundo a partir del bpm
calcHz :: Int -> Int
calcHz bpm = (2 * bpm) `div` 5


-- Calcula los samples por tick a partir del sample rate
-- y de los hz
calcSamplesPerTick :: Int -> Int
calcSamplesPerTick hz = sampleRate `div` hz


-- Utils
applyPan :: Int -> Int16 -> (Int16, Int16)
applyPan p s =
  ((15-fromIntegral p) * s `div` 15, (fromIntegral p*s) `div` 15)
  


note2freq :: Int -> Int
note2freq n = floor $ fromIntegral c * (0.9439**fromIntegral semi)
  where
    (octave, semi) = divMod n 12
    c = 27392 `div` (2^octave)

modFloat :: Float -> Float -> Float
modFloat x y = y * snd (properFraction $ x/y)

apply2 :: (a -> a -> b) -> (a, a) -> (a, a) -> (b, b)
apply2 f (x, y) (z, t) = (f x z, f y t)

dupe :: a -> (a, a)
dupe x = (x, x)
