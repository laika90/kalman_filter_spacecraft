module FileIO where

import Data.List
import Numeric.LinearAlgebra
import System.IO
import Control.Exception(evaluate)

writeData :: [Vector Double] -> [Handle] -> IO ()
writeData [] [] = return ()
writeData state@(x:xs) handlew_list@(hx:hxs) = do
    write x hx
    writeData xs hxs

write :: Vector Double -> Handle -> IO ()
write vec handlew = do
    --let str_vec = show vec
    hPutStrLn handlew (intercalate " " (map show (toList vec)))

readColumn :: FilePath -> Int -> IO [Double]
readColumn fp index = do
    hr <- openFile fp ReadMode
    raw_contents <- hGetContents hr
    let contents = lines raw_contents
    let column = extractColumn contents index
    evaluate (length column)
    hClose hr
    return $ strToDouble column

strToDouble :: [String] -> [Double]
strToDouble = map read

extractColumn :: [String] -> Int -> [String]
extractColumn [] index = []
extractColumn contents@(head:tail) index = do
    getIndex (words head) index : extractColumn tail index

    
getIndex :: [String] -> Int -> String
getIndex str@(s:_) 0 = s
getIndex str@(_:ss) n | n > 0 = getIndex ss (n-1)

readAllData path_list@(fpt:fpw:fpq:_) = do
    t_list  <- readColumn fpt 0
    wx_list <- readColumn fpw 0
    wy_list <- readColumn fpw 1
    wz_list <- readColumn fpw 2
    q0_list <- readColumn fpq 0
    q1_list <- readColumn fpq 1
    q2_list <- readColumn fpq 2
    q3_list <- readColumn fpq 3
    return [t_list, wx_list, wy_list, wz_list, q0_list, q1_list, q2_list, q3_list]

readAllErrorAndVariance path_list@(fpt:fpe:_) = do
    t_list <- readColumn fpt 0
    dwx    <- readColumn fpe 0
    dwy    <- readColumn fpe 1
    dwz    <- readColumn fpe 2
    dq0    <- readColumn fpe 3
    dq1    <- readColumn fpe 4
    dq2    <- readColumn fpe 5
    dq3    <- readColumn fpe 6
    return [t_list, dwx, dwy, dwz, dq0, dq1, dq2, dq3]












