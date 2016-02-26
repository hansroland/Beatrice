import System.IO
import Data.Char
import Data.List.Split

inFileName = "Belege Schweiz L Beenken_bearb.csv"

processFile ::  IO()
processFile = 
    do 
      infile <- openFile inFileName ReadMode
      hSetEncoding infile char8
      -- outfile <- openFile outFileName WriteMode
      inp <- hGetContents infile
      putStrLn (process inp)
      hClose infile
      -- hClose outfile


process :: String-> String
process s = show (length (splitOn [chr 13] s))

