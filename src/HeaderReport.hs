{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HeaderReport(headerReport) where

import Header

import Colonnade
import Data.Char(toUpper)
import Data.List
import qualified Data.Map as M
import Data.Text(Text)
import qualified Data.Text as T
import Fmt
import Numeric(showHex)
import Text.Printf(printf)



instance Buildable Header.Format where
  build Signed   = "signed"
  build Unsigned = "unsigned"

instance Buildable Header where
  build Header{..} =
       nameF "File name" (build fileName)
    <> nameF "Title" (build title)
    <> nameF "Ord number" (build ordNum)
    <> nameF "Song length" (build songLength)
    <> nameF "Number of instruments" (build insNum)
    <> nameF "Number of patterns" (build patNum)
    <> nameF "Tracker" (build tracker)
    <> nameF "Format info" (build formatInfo)
    <> nameF "Global volume" (build globalVol)
    <> nameF "Initial speed" (build initialSpeed)
    <> nameF "Initial tempo" (build initialTempo)
    <> nameF "Stereo flag" (build stereoFlag)
    <> nameF "Master volume" (build masterVolume)
    <> nameF "Default panning" (build defaultPanning)
    <> nameF "Number of channels" (build numChannels)
    <> nameF "Channel info" (M.foldMapWithKey buildNumChannel channels)
    <> nameF "Order" (build order)
    <> nameF "Instruments" (M.foldMapWithKey buildNumInstrument instruments)
    <> nameF "Patterns" (M.foldMapWithKey
                           (buildNumPattern $ sort $ M.keys channels) patterns)


buildNumChannel :: Int -> Channel -> Builder
buildNumChannel n ch = nameF ("Channel num. "+|n|+"") (build ch)


instance Buildable Channel where
  build Channel{..}  =
       nameF "Pan position" (build panPos)
    <> nameF "Default pan" (build defaultPan) 


buildNumInstrument :: Int -> Instrument -> Builder
buildNumInstrument n ins = nameF ("Instrument num. "+|n|+"") (build ins)


instance Buildable Instrument where
  build Instrument{..}  =
       nameF "Instrument type" (build insType)
    <> nameF "Dos file name" (build dosFileName) 
    <> nameF "Mem Seg" (build memSeg) 
    <> nameF "Sample Length" (build sampleLength) 
    <> nameF "Loop begin" (build loopBegin) 
    <> nameF "Loop end" (build loopEnd) 
    <> nameF "Volume" (build insVol) 
    <> nameF "Packing scheme" (build packingScheme) 
    <> nameF "Looped" (build looped) 
    <> nameF "C2SPD" (build c2spd) 
    <> nameF "Sample Name" (build sampleName) 


buildNumPattern :: [Int] -> Int -> Pattern -> Builder
buildNumPattern chs n rs =
  nameF ("Pattern num. "+|n|+"") (build $ reportPattern chs rs)


type MRow    = [Maybe Note]


noteTable :: [String]
noteTable = ["C-","C#","D-","D#","E-","F-","F#","G-","G#","A-","A#","B-"]

noteName :: Int -> String
noteName n = (noteTable!!m) ++ map toUpper (showHex (d+1) "")
  where
    (d, m) = divMod n 12


showNoteName :: Maybe Int -> String
showNoteName Nothing  = "---"
showNoteName (Just n) = noteName n


justifyLeft :: Int -> Char -> String -> String
justifyLeft n c str = replicate (n - length str) c ++ str


showNoteInstrument :: Maybe Int -> String
showNoteInstrument Nothing  = "--"
showNoteInstrument (Just i) = justifyLeft 2 '0' (show i)


showNoteVol :: Maybe Int -> String
showNoteVol Nothing  = "--"
showNoteVol (Just i) = justifyLeft 2 '0' (map toUpper $ showHex i "")

effectTable :: [String]
effectTable = ["A","B","C","D","E","F","G","H","I","J","K","L","M","N",
               "O", "P", "Q", "R", "S0", "S1", "S2","S3","S4","S5",
               "S6","S7","S8","S9","SA","SB","SC","SD","SE","SF","T",
               "U","V","W","X","Y","Z"]

showNoteEffect :: Maybe Int -> Maybe Int -> String
showNoteEffect Nothing Nothing   = "----"
showNoteEffect Nothing (Just p)  =
  "--" ++ justifyLeft 2 '0' (map toUpper $ showHex p "")
showNoteEffect (Just e) Nothing  =
  justifyLeft 2 '0' (effectTable!!(e-1)) ++ "--"
showNoteEffect (Just e) (Just p) =
  justifyLeft 2 '0' (effectTable!!(e-1)) ++
  justifyLeft 2 '0' (map toUpper $ showHex p "")


showNoteInfo :: Maybe Note -> String
showNoteInfo Nothing = "--- -- -- ----"
showNoteInfo (Just Note{..}) = showNoteName note ++ " " ++
                               showNoteInstrument instrument ++ " " ++
                               showNoteVol noteVol ++ " " ++
                               showNoteEffect effect effectParam



colMRowPos :: Int -> Colonnade Headed [Maybe Note] String
colMRowPos n = headed ("Ch " ++ show n) $ showNoteInfo . (!!n)


colMRow :: Int -- Number of channels
        -> Colonnade Headed MRow String
colMRow n = mconcat $ map colMRowPos [0..n-1]



normalizeRow :: [Int] -- Ordered channel list
             -> Row   -- Ordered by channel notes
             -> MRow
normalizeRow [] [] = []
normalizeRow (c:cs) [] = Nothing:normalizeRow cs []
normalizeRow (c:cs) ns'@((n@Note{channel=c'}):ns)
  | c < c'  = Nothing:normalizeRow cs ns'
  | c == c' = Just n:normalizeRow cs ns
  | c > c'  = error "Incoherent channel numbers in a row!"


reportPattern :: [Int]   -- Ordered channel list
              -> Pattern -- Row list
              -> String
reportPattern cs pat = ascii (colMRow nchan) mrows
  where
    pat'   = map (sortOn channel) pat
    nchan  = length cs
    mrows  = map (normalizeRow cs) pat'


headerReport :: Header -> Text
headerReport = pretty

