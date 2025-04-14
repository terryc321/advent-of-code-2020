module Main where

import System.IO

-- True False True && False
-- poor mans parser
isMask :: String -> Bool
isMask s = length s > 4 && take 4 s == "Mask"
--isMask s = length s == 43 && take 4 s == "Mask"

showMasks :: IO ()
showMasks = do
  

  
-- sometimes haskell will load with complaints but still load , just warnings look
-- like it did not load
main :: IO ()
main = do
  fileHandle <- openFile "../input.txt" ReadMode
  contents <- hGetContents fileHandle
  let ls = lines contents 
  -- putStrLn contents
  -- putStrLn str
  -- show length of file
  putStrLn ("file has length of " ++ (show (length contents)) ++ " bytes")
  putStrLn (ls !! 0)
  putStrLn ""
  -- show works sometimes not others , bracketting problem perhaps
  -- putStrLn (show [[(1::Int),2,3],[4,5,6]] )
  showMasks ls
  -- putStrLn ("these are masks " ++ (show (map isMask ls)))
  -- p (\x -> putStrLn ("in>" ++ x)) ls
  hClose fileHandle

  



