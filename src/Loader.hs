
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Loader(loadFile) where

import Header

import qualified Data.Array as A
import Data.Char(isPrint)
import Control.Monad(replicateM, when)
import Control.Monad.Loops(whileJust)
import Data.Binary.Get
import qualified Data.Bits as BT
import qualified Data.ByteString.Lazy as B
import Data.Int
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Encoding(decodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error(lenientDecode)

import Data.Word


loadFile :: FilePath -> IO Header
loadFile file = do
  bytestring <- B.readFile file
  let header = runGet (load file) bytestring
  return header


load :: FilePath -> Get Header
load file = do
  (header, insPtrs, patPtrs) <- lookAhead $ do
    -- signature
    verifySignature

    let fileName = T.pack file

    -- song title
    title <- loadTitle

    -- 0x1A marker
    -- File type (0x10 -> ST3)
    -- 2 bytes for expansion
    skip 4

    -- song length (no preciso, despues se recalcula)
    ordNum <- fromIntegral <$> getWord16le

    -- number of instruments
    insNum <- fromIntegral <$> getWord16le
 
    -- number of patterns (se calcula luego con mas precision)
    patNum <- fromIntegral <$> getWord16le -- OJO comprobar

    -- flags (OJO de momento ignoramos)
    skip 2

    -- tracker version
    tracker <- fromIntegral <$> getWord16le

    -- file format info (unsigned/signed samples)
    formatInfo <- loadFormatInfo

    -- skip signature
    skip 4

    -- global volume
    globalVol <- fromIntegral <$> getWord8
 
    -- initial speed
    initialSpeed <- fromIntegral <$> getWord8
 
    -- initial tempo
    initialTempo <- fromIntegral <$> getWord8

    -- stereo flag and master volume
    (stereoFlag, masterVolume) <- loadMasterVolume

    -- ignore ultraclick removal
    skip 1

    -- default panning
    defaultPanning <- (==252) <$> getWord8

    -- 8 bytes for expansion
    -- 2 bytes for special (custom data, ignore it)
    skip 10

    -- channel settings
    tmpchannels <- loadChannelsSettings
    let numChannels = M.size tmpchannels

    -- order data
    order <- loadOrder ordNum
    let songLength = length order
        -- OJO ver si hay que calcular este aqui
        -- o vale el que se leyo antes
        --patNum   = maximum order + 1

    -- instrument parapointers
    insPtrs <- replicateM insNum $ ((*16) . fromIntegral) <$> getWord16le
  
    -- pattern parapointers
    patPtrs <- replicateM patNum $ ((*16) . fromIntegral) <$> getWord16le

    -- panning information (only used if defaultPaning)
    panningInfo <- replicateM 32 $ (BT..&. 0x0F) <$> getWord8
    let channels = setDefaultPan tmpchannels panningInfo

    let instruments = undefined
        patterns    = undefined

    return (Header{..}, insPtrs, patPtrs)

  -- instruments
  instruments <- loadInstruments insPtrs

  -- pattern data
  patterns <- loadPatterns patPtrs

  return header{instruments=instruments, patterns=patterns}



verifySignature :: Get ()
verifySignature = do
  sig <- lookAhead $ do
    skip 0x2C
    decodeUtf8 <$> getByteString 4
  when (sig /= "SCRM") (error "Bad signature!!")


loadTitle :: Get T.Text
loadTitle = loadText 28


loadFormatInfo :: Get Format
loadFormatInfo = do
  info <- getWord16le
  case info of
    1 -> return Signed
    2 -> return Unsigned


loadMasterVolume :: Get (Bool, Int)
loadMasterVolume = do
  b <- getWord8
  let stereo = BT.shiftR b 7 == 1
      vol    = fromIntegral $ b BT..&. 0x7F
  return (stereo, vol)


loadChannelsSettings :: Get ChannelDic
loadChannelsSettings = do
  chs <- zip [0..] <$> replicateM 32 loadChannelSettings
  let step (n, maybeCh) m = case maybeCh of
                              Nothing -> m
                              Just c  -> M.insert n c m
  return $ foldr step M.empty chs


loadChannelSettings :: Get (Maybe Channel)
loadChannelSettings = channelSettings <$> getWord8


channelSettings :: Word8 -> Maybe Channel
channelSettings b
  | b<16 = Just Channel{panPos=fromIntegral b, defaultPan=undefined}
  | otherwise = Nothing


loadOrder :: Int -> Get [Int]
loadOrder n = do
  os <- replicateM n $ fromIntegral <$> getWord8
  return $ filter (<254) (takeWhile (/=255) os)


setDefaultPan :: ChannelDic -> [Word8] -> ChannelDic
setDefaultPan dic ps =
  M.mapWithKey fn dic
  where
    fn k ch = case lookup k (zip [0..] ps) of
                Just x  -> ch{defaultPan=fromIntegral x}
                Nothing -> ch


loadInstruments :: [Int] -> Get InstrumentDic
loadInstruments ptrs = do
  is <- traverse loadInstrument ptrs
  return $ M.fromList $ zip [0..] is


loadInstrument :: Int -> Get Instrument
loadInstrument ptr = do
  ins <- lookAhead $ do
    skip ptr 

    -- instrument type (3 -> sample)
    insType <- fromIntegral <$> getWord8

    -- DOS file name
    dosFileName <- loadText 12 -- OJO comprobar

    -- ptr to buffer data
    memSeg <- loadMemSeg

    -- sample length (in one word, discard another word)
    sampleLength <- fromIntegral <$> getWord16le
    skip 2
 
    -- loop begin (in one word, discard another word)
    loopBegin <- fromIntegral <$> getWord16le
    skip 2
 
    -- loop end (in one word, discard another word)
    loopEnd <- fromIntegral <$> getWord16le
    skip 2

    -- volume
    insVol <- fromIntegral <$> getWord8

    -- unused byte
    skip 1

    -- packing scheme (deberia ser 0 -> unpacked)
    packingScheme <- fromIntegral <$> getWord8

    -- flags (only load flag 1)
    looped <- ((==1) . (BT..&. 1)) <$> getWord8
 
    -- C2SPD (in one word, discard another word)
    c2spd <- fromIntegral <$> getWord16le
    skip 2

    -- unused bytes
    skip 12

    -- sample name
    sampleName <- loadText 28

    -- verify 'SCRS' (si el instrumento no esta, la sig. tampoco)
    sig <- decodeUtf8 <$> getByteString 4
    --when (sig /= "SCRS") (error $ "Bad signature: " ++ show ptr)

    -- buffer data
    let buffer = undefined

    return Instrument{..}

  lookAhead $ do
    -- salto hasta la posicion del buffer
    skip $ memSeg ins

    let len = sampleLength ins
    ds <- replicateM len getWord8
    return ins{buffer=A.array (0, len-1) $ zip [0..] ds}



loadPatterns :: [Int] -> Get PatternDic
loadPatterns ptrs = do
  -- Filtro los patterns con ptr=0
  let ptrs' = filter ((/=0) . snd) $ zip [0..] ptrs
  is <- traverse loadPattern ptrs'
  return $ M.fromList is


loadPattern :: (Int, Int) -> Get (Int, Pattern)
loadPattern (n, ptr) = do
  lookAhead $ do
    skip ptr

    -- length of the packed pattern data
    skip 2

    (n,) <$> replicateM 64 loadRow


loadRow :: Get Row
loadRow = whileJust loadNote return


loadNote :: Get (Maybe Note)
loadNote = do
  b <- getWord8
  let b1 = b BT..&. 31
      b2 = b BT..&. 32
      b4 = b BT..&. 64
      b8 = b BT..&. 128

  if b>0 then do
    let channel = fromIntegral b1

    (note, instrument) <- if b2>0 then do
                            n <- loadNoteNum
                            i <- fromIntegral <$> getWord8
                            return (Just n, Just i)
                          else
                            return (Nothing, Nothing)

    noteVol <- if b4>0 then do
                 (Just . fromIntegral) <$> getWord8
               else
                 return Nothing

    (effect, effectParam) <- if b8>0 then do
                               e <- fromIntegral <$> getWord8
                               p <- fromIntegral <$> getWord8
                               return (Just e, Just p)
                             else
                               return (Nothing, Nothing)

    return $ Just Note{..}
  else
    return Nothing 


loadText :: Int -> Get T.Text
loadText n = T.takeWhile isPrint <$> text
  where
    text = decodeUtf8With lenientDecode <$> getByteString n


loadMemSeg :: Get Int
loadMemSeg = do
  byte1 <- fromIntegral <$> getWord8
  byte2 <- fromIntegral <$> getWord8
  byte3 <- fromIntegral <$> getWord8

  let ptr = BT.shift byte1 16 + BT.shift byte3 8 + byte2
  return $ ptr*16


loadNoteNum :: Get Int
loadNoteNum = do
  b <- getWord8
  let b' = if b<254 then BT.shift b (-4)*12 + b BT..&. 0x0F else b
  return $ fromIntegral b'

