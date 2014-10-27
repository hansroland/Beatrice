-- Skript um eine CSV Datei mit Weltkoordinaten mit Schweizer Koordinaten
-- zu ergänzen.

-- Die erste Zeile enthält die Spaltenüberschriften.
-- Die Schweizer Koordinaten werden am Anfang jeder Zeile eingefügt.

import Data.Geo.Swiss.Conversion

import Data.List.Split
import System.IO
import Data.Char

-- ---------------------------------------------------------------
-- Constants - instead of a better main programm
-- ---------------------------------------------------------------

inFileName = "Belege Schweiz L Beenken_bearb.csv"
outFileName = "Belege Schweiz L Beenken_bearb_swiss.csv"

-- | Index of Lattitude Field in CSV
ixLatt :: Int
ixLatt = 10

-- | Index of Longitude Field in CSV
ixLong :: Int
ixLong = 9

-- ---------------------------------------------------------------
-- reads the first number in a string
-- ---------------------------------------------------------------
-- do not read ending ' N' or ' E' for North or East
getNumber :: String -> Double
getNumber  = toDouble  . getNumberString
   where
      toDouble xs = read xs :: Double
      getNumberString = takeWhile (\c -> isDigit c || c =='.')


getStrLatt :: [String] -> String
getStrLatt xs = xs !! ixLatt
		 
getStrLong :: [String] -> String
getStrLong xs = xs !! ixLong


getLatt :: [String] -> Double
getLatt = getNumber . getStrLatt
		 
getLong :: [String] -> Double
getLong = getNumber . getStrLong

-- ---------------------------------------------------------------------
-- add the swiss koordinates at the beginning of the string
-- ---------------------------------------------------------------------
addSwissKoord :: String -> String
addSwissKoord xs = asCSV (to03 (wgs2ch wgs84)) ++ xs
     where 
        csv = splitOn ";" xs
        latt = double2Deg $ getLatt csv
        long = double2Deg $ getLong csv
        wgs84 = WGS latt long
		
-- -----------------------------------------------------------------------
-- write out swisskoord with ; in middle and at end
-- -------------------------------------------------------------------
asCSV :: CH03 -> String
asCSV (LV03 x y) = show x ++ ";" ++ show y ++ ";"

-- ---------------------------------------------------------------------
-- Process file
-- ---------------------------------------------------------------------
processFile ::  IO()
processFile = 
    do 
      infile <- openFile inFileName ReadMode
      hSetEncoding infile char8
      outfile <- openFile outFileName WriteMode
      inp <- hGetContents infile
      hPutStr outfile (process inp)
      hClose infile
      hClose outfile

process :: String -> String
process xs = 
      unlines (processArray (myLines xs))

myLines :: String -> [String]
myLines = splitOn [chr 13]

processArray :: [String] -> [String]
processArray (l : ls) =  processFirstLine l : processBody ls

processFirstLine :: String -> String
processFirstLine l = "CH-Laenge 600;CH-Breite;" ++ l

processBody :: [String] -> [String]
processBody = map addSwissKoord

double2Deg :: Double -> Degree
double2Deg d = Deg g m s
    where
      g = floor d
      r1 = (d - fromIntegral g) * 60
      m = floor r1
      s = (r1 - fromIntegral m) * 60


deg2Double :: Degree -> Double
deg2Double (Deg g m s) = fromIntegral g + (fromIntegral m / 60) + (s / 3600)


test01 = "8457;26.06.11;L. Beenken;Aecidium sp.;;L. Beenken;1;;südlich der Thur;8.608333333;47.59138889;360;7;;;Blatt, lebend;von;Clematis vitalba L.;;;;Ludwig Beenken;Thuraue bei Flaach;südlich der Thur;Thuraue bei Flaach,  südlich der Thur;PRIV;2;;unter;Alnus viridis (Chaix) DC.; unter Alnus viridis (Chaix) DC.;Alnus;Russula;alnetorum;;"
