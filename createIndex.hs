-- -------------------------------------------------------------------
-- Skript zum Erstellen eines CSV Files mit Filenamen; Pilznamen
-- -------------------------------------------------------------------
--
-- Umbennenen von Pilzfotos für Beatrice
--
-- Input: Verzeichnis mit Filenamen der Art:
--        JG0001 Agaricus campestris.jpg
--        JG0223 Cortinarius (Phl.) cyanobasalis.jpg
--
-- Output: CSV Datei mit Filenamen und lateinischem Namen 
--
-- Zuerst muss mit dem Script Rename.hs das Umbenennen durchgeführt werden.
--
--
-- Das Verzeichnis ist hart in der Funktion fromDir hinterlegt
--
-- -------------------------------------------------------------------
import System.Directory(getDirectoryContents, copyFile, renameFile)
import System.IO
import Data.List(isSuffixOf, sort)
import Data.Char (toUpper)

main :: IO()
main = createIndex fromDir

-- Directory, in dem die Files gelesen werden
fromDir :: FilePath
-- fromDir = "/home/roland/Beatrice/"
fromDir = "/home/roland/Beatrice/Wilhlem_Markus_2013_Bilder/Renames/"


-- Hauptverarbeitung:
-- Alle Filenamen im Direcotry werden gelesen
-- Es werden die .jpg Files herausgefiltert
-- Der lateinische namen wird extrahiert
createIndex :: FilePath -> IO()
createIndex path = do
  hFile <- openFile (fromDir ++ "index.csv") WriteMode
  files <- getDirectoryContents path
  let lines = map getLatName $ sort $ selectJpgs files
  mapM_ (hPutStrLn hFile) lines
    where
      selectJpgs = filter $ isSuffixOf ".JPG". map toUpper

-- Entfernt das .jpg am Ende der Datei
removeTrailingJPG :: String -> String
removeTrailingJPG  = reverse . drop 4 . reverse

-- Entfernt die LaufNr am Anfang
removeHeader :: String -> String
removeHeader = drop 7

removeMultBlanks :: String -> String
removeMultBlanks [] = []
removeMultBlanks (' ' : []) = []
removeMultBlanks (' ' : ' ' : xs) = removeMultBlanks (' ' :xs)
removeMultBlanks (x : xs) = x: removeMultBlanks xs

-- Entfernt den Text in Klammern
removeParens :: String -> String
-- removeParens = (takeWhile /= lParen)
-- removeParens =  (dropWhile (/= ')')) . (takeWhile (/= '('))
removeParens fn =  first ++ last
  where
     first = takeWhile (/= '(') fn
     last = if null last' then [] else tail last'
     last' = dropWhile (/= ')') fn
    
createCSVLine :: String -> String
createCSVLine = removeMultBlanks . removeParens . removeTrailingJPG . removeHeader

getLatName :: String -> String
getLatName  fn =  fn ++ ";" ++ createCSVLine fn ++ ";"
