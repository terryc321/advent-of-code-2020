
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Typeable -- typeOf
import System.IO

-- import Data.String

-- for reference i used this 
-- https://williamyaoh.com/posts/2019-04-11-cheatsheet-to-regexes-in-haskell.html
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
                   in map (\x -> read x :: Integer)
                      (getAllTextMatches ((str :: String) =~ (re::String))::[String])

-- avoid maybe's like plague

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
collect (h:t) acc = collect2 t (maskCode h) [] acc

--collect2 lines mask mems acc
collect2 [] mask mems acc = acc ++ [[(mask,mems)]]                            
collect2 (h:t) mask mems acc =
  if isMask h then collect (h:t) (acc ++ [[(mask,mems)]])
  else let mem = extractInts h -- found a mem here
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

-- https://stackoverflow.com/questions/9166148/how-to-implement-decimal-to-binary-conversion
-- extra step binarz 0 produced [] empty list , so fixed it to make it make [0] instead
-- other than that just a copy
binarz :: Integer -> [Integer]
binarz 0 = [0]
binarz x = binarz2 x 

binarz2 :: Integer -> [Integer]
binarz2 0 = []
binarz2 n = binarz2 (div n 2) ++ [(mod n 2)] 

-- pads list to 36 elements long if not already
pad36 xs = let len = length xs
               diff = 36 - len
           in (paddiff diff) ++ xs

paddiff 0 = []
paddiff n = 0 : (paddiff (n - 1))

-- pad an integer to binary representation to 36 elements long
encodeVal n = pad36 (binarz n)

--- combineMask mask encoded-value -> masked-value
--- mask head mh tail mt ,encoded value head eh tail et
--- combineMask (mh:mt) (eh:et) =  
combineMask [] [] = []
combineMask ('X':mt) (eh:et) =  eh : (combineMask mt et)
combineMask ('0':mt) (eh:et) =  0 : (combineMask mt et)
combineMask ('1':mt) (eh:et) =  1 : (combineMask mt et)

compute mask val = combineMask mask (encodeVal val)

-- given mask value convert to single number
-- list rep -> integer
maskValue2 [] p s = s
maskValue2 (0:t) p s = maskValue2 t (p*2) s
maskValue2 (1:t) p s = maskValue2 t (p*2) (s + p)

maskValue xs = maskValue2 (reverse xs) 1 0

-- test driven development
m1 = maskValue (compute "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 11)
m2 = maskValue (compute "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 101)
m3 = maskValue (compute "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X" 0)
-- expect m1 = 73
-- expect m2 = 101
-- expect m3 = 64

-- mem[8]=11
-- mem 8 is the destination unchanged , 11 is masked then value computed from list rep

---
-- mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
-- beforeVal = 11 
-- dest 8

-- runOrder takes this data
-- [("00000X110010111111X000100XX01010000X",[[20690,435],[54036,231],[27099,118644255],[55683,22299263],[26119,2279399]])]
-- and makes this 
-- [("00000X110010111111X000100XX01010000X",[20690,435]
-- [("00000X110010111111X000100XX01010000X",[54036,231]
-- [("00000X110010111111X000100XX01010000X",[27099,118644255]
-- [("00000X110010111111X000100XX01010000X",[55683,22299263]
-- [("00000X110010111111X000100XX01010000X",[26119,2279399]

-- acc is where we put the result
-- runOrder3 mask mems acc = 

runOrder3 mask [] acc = acc
runOrder3 mask ([a,b]:mt) acc = runOrder3 mask mt (acc ++ [(mask,a,b)])
runOrder2 (mask,mems) = runOrder3 mask mems []
runOrder xs = runOrder2 (head xs)


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

-- memory space is written to
-- format (destination , value )
-- if destination occurs later in the list , then it will get a new value then
-- so no point in doing anything with it now because its value in memory space is
-- overwritten later on
--
-- otherwise nobody write to this memory location later , so this is the value it
-- has at end of the run
-- so sum it up
--
-- if no more values then the result is the sum s
-- we are done
memSpace :: [(Integer,Integer)] -> Integer -> Integer  
memSpace [] s = s
memSpace ((d,v):t) s = if memIn d t then memSpace t s
                       else memSpace t (s+v)

-- does destination d occur later on in the memory writes                            
memIn d [] = False
memIn d ((d2,v2):t) = if d == d2 then True
                      else memIn d t
                          





  
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
  let rc = runCollect ls
  let pc = map runOrder rc
  
  mapM_ (\x -> do print x
                  putStrLn "") rc 

  putStrLn "-- input to runOrder ---"
  print (head rc)
  putStrLn "-- lets see if we can do this with runOrder ---"
  mapM_ (\x -> do print x
                  putStrLn "") pc 
  putStrLn "--- end of runOrder ---"

  -- f is a list of mask dest val 
  -- dest : where value eventually written
  -- val  : value before converted to 36 bit and masked
  -- mask : string 36 chars long represent the 36 bit mask
  let f = foldr (++) [] pc
  mapM_ (\x -> do print x ) f 

  putStrLn "--- end of f ---"

  -- g 
  let g = map (\(mask,dest,val) -> (dest,maskValue (compute mask val))) f
  mapM_ (\x -> do print x ) g 
  
  putStrLn "--- end of g ---"

  putStrLn "--- tot computed ---"
  let tot = memSpace g 0
  putStrLn ("--- tot = " ++ (show tot))
  putStrLn "--- tot end ---"
-- --- tot computed ---
-- --- tot = 4297467072083
-- --- tot end ---
-- ghci> binarz 4297467072083
-- [1,1,1,1,1,0,1,0,0,0,1,0,0,1,0,1,0,0,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,0,0,1,0,1,0,0,1,1]
-- ghci> length (binarz 4297467072083)
-- 42
    
  -- putStrLn ("these are masks " ++ (show (map isMask ls)))
  -- p (\x -> putStrLn ("in>" ++ x)) ls
  hClose fileHandle
  


  
  



