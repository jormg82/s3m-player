{-# LANGUAGE RecordWildCards #-}

module Player(playFile, interpolate, retarg) where

import qualified Header as H


import Control.Monad(filterM, replicateM_, when)
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


type Stream = [Float]
type Stream2 = [(Float, Float)]


-- Constantes
zero :: Float
zero = 0

zero2 :: (Float, Float)
zero2 = (0, 0)

sampleRate:: Int
sampleRate = 11025



-- Datos
data ChState = ChState
  {
    instrument   :: Maybe Int,
    samplePos    :: Float,
    frequence    :: Int,
    volume       :: Int,
    pan          :: Int,
    effect       :: Maybe Int,
    effectParam  :: Maybe Int,
    lastVolSlide :: Int
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
    samplePos    = 0,
    instrument   = Nothing,
    frequence    = 0,
    volume       = 64,
    pan          = 7,
    effect       = Nothing,
    effectParam  = Nothing,
    lastVolSlide = 0
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
  traverse_ updateChNote validNotes

  speed <- getSpeed
  replicateM_ speed playTick


updateChNote :: H.Note -> PS ()
updateChNote H.Note{..} = do

  chstate <- getChannelState channel
  case instrument of
    Nothing -> return ()
    Just i  -> putChState channel chstate{instrument=Just i,
                                          samplePos=0}

  chstate <- getChannelState channel
  let vol = case noteVol of
              Nothing -> volume chstate
              Just v  -> v
  putChState channel chstate{volume=vol,
                             effect=effect,
                             effectParam=effectParam}

  chstate <- getChannelState channel
  case note of
    Nothing -> return ()
    Just n -> putChState channel chstate{frequence=note2freq n}

  chstate <- getChannelState channel
  case (effect, effectParam) of

    -- Axx (Set Speed)
    (Just 1, Just speed) -> do
      putSpeed speed

    -- Dxy (Volume slide)
    (Just 4, Just p) -> do
      let up     = BT.shift p (-4)
          down   = p BT..&. 0x0F
          lastVS = max p (lastVolSlide chstate)
          vol    = case (up, down) of
                     (_, 0x0F) -> volume chstate+up
                     (0x0F, _) -> volume chstate-down
                     _         -> volume chstate
          vol'   = adjustLimits 0 64 vol
      putChState channel chstate{volume=vol', lastVolSlide=lastVS}

    -- Oxy (Sample offset)
    (Just 15, Just o) -> do
      putChState channel chstate{samplePos=fromIntegral (o*0x100)}

    -- SX- effects
    (Just 19, Just o) -> do
      let up   = BT.shift o (-4)
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
  chTicks <- (zip [0..] . M.keys . H.channels) <$> getHeader
  -- generamos la minima mezcla posible
  stream <- mix <$> traverse playChTick chTicks
  liftIO $ B.hPut stdout $ B.pack stream


playChTick :: (Int, Int) -> PS Stream2
playChTick (nchan, numTick) = do
  spt <- getSamplesPerTick
  chstate@ChState{..} <- getChannelState nchan
  H.Header{..} <- getHeader

  -- En los ticks intermedios,
  -- aplicamos efectos antes de producir samples
  when (numTick>0) $ updateChTick nchan

  --liftIO $ putStrLn $ "Aqui volume vale: " ++ show volume

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
          funVol :: Float -> Float
          funVol s = s * fromIntegral (globalVol*insVol*volume) / (64*64*64)
          samples' = map funVol samples 

          -- aplicamos panning
          samples'' = map (applyPan pan) samples'
 
      putChState nchan chstate{samplePos=nextpos}       
      return samples''


updateChTick :: Int -> PS ()
updateChTick nchan = do
  chstate@ChState{..} <- getChannelState nchan
  case (effect, effectParam) of
 
    -- Dxy (Volume slide)
    (Just 4, Just _) -> do
      let up     = BT.shift lastVolSlide (-4)
          down   = lastVolSlide BT..&. 0x0F
          vol    = case (up, down) of
                     (_, 0x00) -> volume+up
                     (0x00, _) -> volume-down
          vol'   = adjustLimits 0 64 vol
      putChState nchan chstate{volume=vol'}

    -- default case (no implementado o no existente)
    _ -> return ()


interpolate :: A.Array Int Float -- Buffer
            -> Int               -- Buffer length
            -> Float             -- position (0<=, <buffer length)
            -> Float
interpolate buffer len pos =
  if pos > fromIntegral len then zero
  else f1 + (f2-f1) * snd (properFraction pos)
  where
    b1 = floor pos `mod` len
    b2 = ceiling pos `mod` len 
    f1 = buffer A.! b1
    f2 = buffer A.! b2

retarg :: Bool -> Float -> Float -> Float -> Float
retarg looped begin end pos
  | pos < end || not looped = pos
  | otherwise = begin+modFloat (pos-begin) (end-begin)


mix :: [Stream2] -> [Word8]
mix = map H.float2word8
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
adjustLimits :: Ord a => a -> a -> a -> a
adjustLimits down up val
  | val < down = down
  | val > up   = up
  | otherwise  = val


applyPan :: Int -> Float -> (Float, Float)
applyPan p s =
  ((15-fromIntegral p) * s / 15, (fromIntegral p*s) / 15)


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

