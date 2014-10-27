-- -------------------------------------------------------------------
-- Skript zum Umbenennen von Fotos
-- -------------------------------------------------------------------
--
-- Umbennenen von Pilzfotos für Beatrice
--
-- Input: Verzeichnis mit Filenamen der Art:
--        Agaricus campestris-Feld-Champignon-5.JPG
--
-- Output: Diese Filenamen werden geändert auf
--         JG0001 Agaricus campestris.jpg
--           wobei hinter JG (für Juerg Gilgen eine fortlaufende 
--           Nr folgt.
--
-- Je nach Funktion in processPair werden die Fotos kopiert oder
--         umbenannt.
--
-- Das Verzeichnis ist hart in der Funktion fromDir hinterlegt
--
-- -------------------------------------------------------------------
import System.Directory(getDirectoryContents, copyFile, renameFile)
import Data.List(isSuffixOf, sort)
import Data.Char (toUpper)

main :: IO()
main = copyAndRenameFiles fromDir

-- Directory, in dem die .jpg Files gelesen werden
fromDir :: FilePath
-- fromDir = "/home/roland/Beatrice/"
fromDir = "/home/roland/Beatrice/Wilhlem_Markus_2013_Bilder/"

-- Directory, in das die Kopien geschrieben werden
toDir :: FilePath
toDir = fromDir ++ "Renames/"

-- Hauptverarbeitung:
-- Alle Filenamen im Direcotry werden gelesen
-- Es werden die .jpg Files herausgefiltert
-- Die Filenamen werden sortiert
-- Es wird eine Liste von Paaren mit den alten und neuen Filenamen erstellt
-- Die Datien werden umbenennt / kopiert
copyAndRenameFiles :: FilePath -> IO()
copyAndRenameFiles path = do
  files <- getDirectoryContents path
  mapM_ processPair $ (newNames . sort . selectJpgs ) files
    where 
      selectJpgs = filter $ isSuffixOf ".JPG". map toUpper

-- Creates from a list of filenames a list of pairs with
-- old filename and new filename.
-- Start with a sprcial first number
newNames :: [String] -> [(String, String)]
newNames = lnewNames 0
    where
      lnewNames n []       = []
      lnewNames n (f : fs) = (f, newName n f) : lnewNames (n + 1) fs

-- create a new name from an integer and an existing filename
newName :: Int -> String -> String
newName n fn = "MW" ++ strNum n ++ " " ++ takeWhile (/= '-') fn ++ ".jpg"
   where 
     strNum :: Int -> String 
     strNum n = tail $ show $ 10000 + n

-- Ein Paar bestehend aus einem alten und neuen Filnamen wird verarbeitet.
-- Kopieren oder Umbenennen
processPair :: (String, String) -> IO()
-- processPair (old, new) = renameFile (fromDir ++ old) (fromDir ++ new)
processPair (old, new) = copyFile (fromDir ++ old) (toDir ++ new)
