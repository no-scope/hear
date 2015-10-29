module Hear.Music where

import qualified Data.Vector as V

import Euterpea

type Interval = (AbsPitch, AbsPitch)

intervalName :: Int -> String
intervalName x
  | x < 0     = "Descending " ++ go (-x)
  | x == 0    = "Unison"
  | otherwise = "Ascending " ++ go x
    where
        go y = intervals !! (y `mod` 12)
        intervals = ["Octave",
                     "Minor Second",
                     "Major Second",
                     "Minor Third",
                     "Major Third",
                     "Perfect Fourth",
                     "Diminished Fifth / Augmented Fourth",
                     "Perfect Fifth",
                     "Augmented Fifth / Minor Sixth",
                     "Major Sixth",
                     "Minor Seventh",
                     "Major Seventh"
                    ]


tones :: V.Vector String
tones = V.fromList $ ["A0", "A#0", "B0"] ++ (app' <$> ['1'..'7'] <*> octave) ++ ["C8"]
    where app' x y = y ++ [x]
          octave = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]


scale :: [Int] -> [AbsPitch]
scale pattern = go
  where go = pattern ++ map  (+12) go


majorScale :: [AbsPitch]
majorScale = scale [0, 2, 4, 5, 7, 9, 11]


minorScale :: [AbsPitch]
minorScale = scale [0, 2, 3, 5, 7, 8, 10]


trans' :: Pitch -> Int -> Pitch
trans' origPitch i = pitch $ absPitch origPitch + i


--solfej :: AbsPitch -> AbsPitch -> [AbsPitch]
--solfej x y
--  | y `elem` major = [z | z <- major, z <= max x y, z >= min x y]
--  | y `elem` minor = [z | z <- minor, z <= max x y, z >= min x y]
--  | otherwise = tonePath x y
--    where root = pitch x
--          pitchy = pitch y
--          major = trans flip majorScale
--          minor = trans minorScale


toMusic :: [AbsPitch] -> Music Pitch
toMusic = line . map (note (1/2) . pitch)


tonePath ::  Interval -> [AbsPitch]
tonePath (x, y)
  | x == y         = [y]
  | x - y == 1     = [y, x]
  | y - x == 1     = [x, y]
  | x < y          = x : tonePath (x+2, y)
  | x > y          = x : tonePath (x-2, y)
  | otherwise      = tonePath (x,y)
