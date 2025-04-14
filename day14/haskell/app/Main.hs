
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Typeable -- typeOf
import System.IO

-- import Data.String

-- added regular expression for integer extraction
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Text.Regex.TDFA.String ()

-- import Text.Regex.TDFA ((=~))
-- import Text.Regex.TDFA.String ()  -- just to make sure the String instance is available

-- this is all to learn how to get 2 numbers from a string in haskell , painful...
-- haskell is well confused about how to solve String , ByteString , Text ... whatever
mailRegex = "[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+" :: String
mailRegex1 :: Bool
mailRegex1 = ("my email is email@email.com" :: String) =~ mailRegex 


-- --- this is fine in the repl , but balks in file code
-- "div[attr=1234]" =~ "div\\[([a-z]+)=([^]]+)\\]" :: (String, String, String, [String])
-- -- as is this
-- getAllTextMatches ("john anne yifan" =~ "[a-z]+") :: [String]

fooRe = "[a-z]+" :: String
foo :: [String]
foo = getAllTextMatches (("john anne yifan" :: String) =~ fooRe)

-- in order to get this into one line it needs a fair amount of type declaration
-- otherwise it will balk seriously
foo2 = getAllTextMatches (("john anne yifan" :: String) =~ ("[a-z]+" :: String)) :: [String]

-- ok so finally finished getting some integers from a string - at last
extractInts str =  let re = "[0-9]+" 
                   in map (\x -> read x :: Int)
                      (getAllTextMatches ((str :: String) =~ (re::String))::[String])
                      

                      

                                                                                 
-- extractInts :: String -> Maybe (Int, Int)
-- extractInts str =
--   case (str =~ "(-?[0-9]+).*?(-?[0-9]+)" :: [[String]]) of
--     [[_, a, b]] -> Just (read a, read b)
--     _           -> Nothing

-- extractInts :: String -> Maybe (Int, Int)
-- extractInts str =
--   let matches :: [[String]]
--       matches = str =~ "(-?[0-9]+).*?(-?[0-9]+)"
--   in case matches of
--        [[_, a, b]] -> Just (read a, read b)
--        _           -> Nothing


-- extractInts :: String -> Maybe (Int, Int)
-- extractInts str =
--   let re = "^.*?(-?[0-9]+).*?(-?[0-9]+)$" 
--   in  case (str =~ re :: (String, String, String, [String])) of
--       (_, _, _, [a, b]) -> Just (read a, read b)
--       _                 -> Nothing

    
-- True False True && False
-- poor mans parser
isMask :: String -> Bool
-- moved mysteriously from uppercase Mask to lowercase mask
--isMask s = length s > 4 && take 4 s == "mask"
isMask s = length s == 43 && take 4 s == "mask"

maskCode :: String -> String
maskCode s = drop 7 s

-- indentation is important in haskell

-- total problem input stats
-- 100 masks
-- 464 mems 

-- want mask to be associated with a series of mem structures in a list
-- keep order as it MAY be important , append ++ if have to

-- collect :: [String] -> [String] -> [[String]]
-- collect [] [] acc = acc
-- collect [] x acc = acc ++ [[x]]
-- collect (h : t) x acc = if isMask h then collect t [h] (acc ++ [[x]])
--                                     else collect t (x ++ [h]) acc
--
-- collect [] acc = acc
-- collect (h : t) acc = if isMask h then collect t ([h] : acc)
--                       else collect t ((head acc) )

--- assume mask is well formed and comes first
--- collect lines acc 
collect [] acc = acc
collect (h:t) acc = collect2 t h [] acc


--collect2 lines mask mems acc
collect2 [] mask mems acc = acc ++ [[(mask,mems)]]                            
collect2 (h:t) mask mems acc =
  if isMask h then collect (h:t) (acc ++ [[(mask,mems)]])
  else let mem = h
       in collect2 t mask (mems ++ [mem]) acc                     

-- (define collect2 (lambda (lines mask mems acc)		   
-- 		   (cond  ;; end of current mask - may have zero mems associated 
-- 		    ((or (null? lines) (mask? (car lines)))
-- 		     ;; (format #t "mask = ~a~%" mask)
-- 		     ;; (pp mems)
-- 		     ;; (format #t "~%")
-- 		     (let ((acc2 (append acc (list (list mask mems)))))
-- 		       (collect lines acc2)))
-- 		    (#t ;; found a mem to attach to mask
-- 		     (incf! count-mems)
-- 		     (let ((mem (extract-mem (car lines))))
-- 		       (collect2 (cdr lines) mask (append mems (list mem)) acc))))))
-- (define (run-collect)
--   (collect input '()))
runCollect input = collect input []
                                         

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
  -- mapM without underscore is monadic keep results
  fred <- mapM print masks
  putStrLn ("fred = " ++ (show fred))
  
  
-- sometimes haskell will load with complaints but still load , just warnings look
-- like it did not load
-- main :: IO ()
main = do
  fileHandle <- openFile "../input.txt" ReadMode
  contents <- hGetContents fileHandle
  let ls = lines contents
  print (typeOf ls)
  print (length contents)
  -- putStrLn contents
  -- putStrLn str
  -- show length of file
  putStrLn ("file has length of " ++ (show (length contents)) ++ " bytes")
  putStrLn (ls !! 0)
  putStrLn ""
  -- show works sometimes not others , bracketting problem perhaps
  -- putStrLn (show [[(1::Int),2,3],[4,5,6]] )
  showMasks ls
  putStrLn "lets see if we can do this with runCollect"
  mapM_ (\x -> do print x
                  putStrLn "") (runCollect ls)
    
  -- putStrLn ("these are masks " ++ (show (map isMask ls)))
  -- p (\x -> putStrLn ("in>" ++ x)) ls
  hClose fileHandle
  

  
  



