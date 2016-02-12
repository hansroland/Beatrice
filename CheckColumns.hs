-- Skript um aus einer CSV Datei mit Weltkoordinaten die Weltkoordinaten zu
-- extrahieren.
-- Das Skript dient vor allem dazu zu prüfen, ob das CSV File sauber ist
-- und alle Weltkoordinaten in der gleichen Splate sind.
-- Das Input File wird gelesen, und ein Output File mit den beiden
-- angegeben Spalten wird erstellt


-- Die erste Zeile enthält die Spaltenüberschriften.
-- Die Schweizer Koordinaten werden am Anfang jeder Zeile eingefügt.

-- import Data.Geo.Swiss.Conversion

import Data.List.Split
import System.IO
import Data.Char

-- ---------------------------------------------------------------
-- Constants - instead of a better main programm
-- ---------------------------------------------------------------

inFileName = "Ludwig2.csv"
outFileName = "out2.csv"

-- | Zero based Index of Lattitude Field in CSV
ixLatt :: Int
ixLatt = 10

-- | Zero base Index of Longitude Field in CSV
ixLong :: Int
ixLong = 9

getStrLatt :: [String] -> String
getStrLatt xs = xs !! ixLatt

getStrLong :: [String] -> String
getStrLong xs = xs !! ixLong

-- | Trennzeichen für Zeilen
newLine :: Int
newLine = 10                            -- ev mit 13 versuchen !!
-- ---------------------------------------------------------------------
-- add the swiss koordinates at the beginning of the string
-- ---------------------------------------------------------------------
addSwissKoord :: String -> String
addSwissKoord [] = []
addSwissKoord xs = addToCSV latt long
     where
        csv = splitOn ";" xs
        latt = getStrLatt csv
        long = getStrLong csv
-- -----------------------------------------------------------------------
-- write out swisskoord with ; in middle and at end
-- -------------------------------------------------------------------
addToCSV :: String -> String -> String
addToCSV strLatt strLong = strLatt ++ ";" ++ strLong ++ ";"

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
myLines = splitOn [chr newLine]

processArray :: [String] -> [String]
processArray (l : ls) =  processFirstLine l : processBody ls

processFirstLine :: String -> String
processFirstLine l = "CH-Laenge 600;CH-Breite;" ++ l

processBody :: [String] -> [String]
processBody = map addSwissKoord


test01 = "8457;26.06.11;L. Beenken;Aecidium sp.;;L. Beenken;1;;südlich der Thur;8.608333333;47.59138889;360;7;;;Blatt, lebend;von;Clematis vitalba L.;;;;Ludwig Beenken;Thuraue bei Flaach;südlich der Thur;Thuraue bei Flaach,  südlich der Thur;PRIV;2;;unter;Alnus viridis (Chaix) DC.; unter Alnus viridis (Chaix) DC.;Alnus;Russula;alnetorum;;"
