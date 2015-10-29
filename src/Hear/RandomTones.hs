module Hear.RandomTones where

import           System.Random

import           Hear.Music

randTones :: Int -> (Int, Int) -> [Int] -> IO [Interval]
randTones n range intervals = go n []

  where go :: Int -> [Interval] -> IO [Interval]
        go 0 acc = return acc
        go m acc = do seed <- newStdGen
                      let xs = takeTwo intervals $ randomRs range seed
                      go (m-1) (xs:acc)

        takeTwo :: [Int]-> [Int]-> Interval
        takeTwo _      []          = (-1, -1)
        takeTwo _      [_]         = (-1, -1)
        takeTwo []     (x : y : _) = (x, y)
        takeTwo inters (x : y : xs)
          | abs (x-y) `elem` inters = (x, y)
          | otherwise               = takeTwo intervals xs
