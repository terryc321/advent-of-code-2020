

module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"

notes2 :: (Int , [Int])
notes2 = (1000391 , filter (\x -> x /= 0) (let x = 0 in [19,x,x,x,x,x,x,x,x,x,x,x,x,37,x,x,x,x,x,383,x,x,x,x,x,x,x,23,x,x,x,x,13,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29,x,457,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,17]))

notes :: (Int , [Int])
notes = (939,filter (\x -> x /= 0) (let x = 0 in [7,13,x,x,59,x,31,19]))

-- next bus
next :: Int -> Int -> Int
next t n = let b = t `div` n
           in let c = b * n
              in if c < t then (b+1)*n
                 else b*n

minwait :: (Int,Int) -> [(Int,Int)] -> (Int,Int)
minwait a [] = a
minwait (a,n) ((b,m) : []) = if n < m then (a,n) else (b,m)
minwait (a,n) ((b,m) : xs) = if n < m then minwait (a,n) xs else minwait (b,m) xs

                                                                 
-- returns id , time to next bus
findNext :: (Int , [Int]) -> [(Int,Int)]
findNext (t,xs) = map (\n -> (n,(next t n) - t)) xs

part1 :: (Int, Int , Int)
part1 = let buses = findNext notes2
        in let h = head buses
               t = tail buses
               in let (e,r) = minwait h t
                  in (e,r,e*r)

-- (383,5,1915)
-- ACCEPTED ! 

-- t = 7
-- t + 1 = 13
-- x,
-- x,
-- t+4 = 59,x,
-- t+6 = 31
-- t+7 = 19



                  







