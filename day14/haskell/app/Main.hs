module Main where

import System.IO

-- True False True && False
-- poor mans parser
isMask :: String -> Bool
-- moved mysteriously from uppercase Mask to lowercase mask
--isMask s = length s > 4 && take 4 s == "mask"
isMask s = length s == 43 && take 4 s == "mask"

maskCode :: String -> String
maskCode s = drop 5 s



-- total problem input stats
-- 100 masks
-- 464 mems 

-- print and show are closely related 
-- ghci> :t print
-- print :: Show a => a -> IO ()
-- ghci> :t show
-- show :: Show a => a -> String
showMasks :: [String]-> IO ()
showMasks ls = do
  putStrLn "showmasks doing something"
  -- map (\x -> if isMask x then putStrLn (show x) else putStr "" ) ls
  let masks = filter isMask ls
  let masksCount = length masks
  putStrLn ("There are " ++ (show masksCount) ++ " masks found.")
  -- mapM_ (\x -> putStrLn (show x)) masks
  -- mapM_ is monadic do something to each element and discard results
  mapM_ print masks
  mapM_ print masks
  
  -- mapM without underscore is monadic keep results
  fred <- mapM print masks
  putStrLn ("fred = " ++ (show fred))
  
  
  

  
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
  -- mapM_ print ls
  
  -- putStrLn ("these are masks " ++ (show (map isMask ls)))
  -- p (\x -> putStrLn ("in>" ++ x)) ls
  hClose fileHandle

  



