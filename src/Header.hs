
module Header(
  Format(..),
  Channel(..),
  ChannelDic,
  InstrumentDic,
  PatternDic,
  Pattern,
  Row,
  Note(..),
  Instrument(..),
  Header(..)
) where


import qualified Data.Array as A
import qualified Data.Map as M
import Data.Text(Text)
import Data.Word


data Format = Signed | Unsigned
              deriving Show


type ChannelDic    = M.Map Int Channel
type InstrumentDic = M.Map Int Instrument
type PatternDic    = M.Map Int Pattern

type Pattern = [Row]
type Row     = [Note]


data Channel = Channel
  {
    panPos     :: Int,
    defaultPan :: Int
  }
  deriving Show

data Note = Note
  {
    channel     :: Int,
    note        :: Maybe Int,
    instrument  :: Maybe Int,
    noteVol     :: Maybe Int,
    effect      :: Maybe Int,
    effectParam :: Maybe Int
  }
  deriving Show

data Instrument = Instrument
  {
    insType       :: Int,
    dosFileName   :: Text,
    memSeg        :: Int,
    sampleLength  :: Int,
    loopBegin     :: Int,
    loopEnd       :: Int,
    insVol        :: Int,
    packingScheme :: Int,
    looped        :: Bool,
    c2spd         :: Int,
    sampleName    :: Text,
    buffer        :: A.Array Int Word8
  }
  deriving Show

data Header = Header
  {
    fileName       :: Text,
    title          :: Text,
    ordNum         :: Int,
    songLength     :: Int,
    insNum         :: Int,
    patNum         :: Int,
    tracker        :: Word16,
    formatInfo     :: Format,
    globalVol      :: Int,
    initialSpeed   :: Int,
    initialTempo   :: Int,
    stereoFlag     :: Bool,
    masterVolume   :: Int,    -- only SoundBlaster?
    defaultPanning :: Bool,
    numChannels    :: Int,
    channels       :: ChannelDic,
    order          :: [Int],
    instruments    :: M.Map Int Instrument,
    patterns       :: M.Map Int Pattern
  }
  deriving Show

