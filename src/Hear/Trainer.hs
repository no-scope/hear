module Hear.Trainer where


import           Text.Printf
import           Text.Read

import           System.Exit
import           System.IO
import           System.Console.ANSI

import           Control.Exception

import           Data.Maybe

import           Hear.RandomTones
import           Hear.Music

import           Euterpea


clearFromLine :: Int -> IO()
clearFromLine n = do
  setCursorPosition n 0
  clearFromCursorToScreenEnd


playInterval :: Interval -> IO()
playInterval (x, y) = play . line $ map (note (1/2)) [pitch x, pitch y]


withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action


printAns :: Interval -> IO()
printAns (x, y) = do
  let diff = y - x
  putStrLn ""
  printf "Midi Numbers          : "
  print (x, y)
  printf "Notes                 : "
  print (pitch x, pitch y)
  printf "Interval in Semitones : %d, %s\n" diff (intervalName diff)


freePractise :: IO()
freePractise = do
  clearFromLine 0
  putStrLn helpstring
  tns <- randTones 1 (60, 70) []
  go $ head tns
  where
    helpstring = "p play tones   s sing the tones between the interval   a show answer   q quit"

    go :: Interval -> IO()
    go tnl = do
      tmp <- withEcho False getChar
      case tmp of
        'p' -> playInterval tnl
        'n' -> do xs <- randTones 1 (40, 80) []
                  go $ head xs
        'a' -> printAns tnl
        's' -> play $ (toMusic . tonePath) tnl
        'q' -> exitSuccess
        _   -> putStrLn "press h for available commands"
      go tnl


intervalTest :: Int -> [Int] -> IO()
intervalTest x intervals = do
  clearFromLine 0
  putStrLn helpstring
  tns <- randTones x (40, 80) intervals
  result <- mapM (`go` Nothing) tns
  let rightAns = length $ filter (== True) result
  printf "Score : %d / %d\n" rightAns x
  where
    helpstring ="p play tones   a give answer   q quit"

    go :: Interval -> Maybe Int -> IO Bool
    go tnl ans
          | isNothing ans = do
            clearFromLine 1
            tmp <- withEcho False getChar
            case tmp of
              'p' -> do
                playInterval tnl
                go tnl Nothing
              'a' -> do
                new_ans <- takeAns
                go tnl new_ans
              'q' -> exitSuccess
              _   -> go tnl Nothing
          | otherwise = do
            result <- checkAns $ fromJust ans
            putStrLn "any key for next question"
            _ <- withEcho False getChar
            return result

            where
              checkAns :: AbsPitch -> IO Bool
              checkAns answer
                | answer == correct_ans = do
                  putStrLn "Correct!"
                  printAns tnl
                  return True
                | otherwise = do
                  putStrLn "Nope!"
                  printAns tnl
                  return False

              correct_ans = snd tnl - fst tnl


takeAns :: IO (Maybe Int)
takeAns = do
  putStr "Answer:"
  tmp <- getLine
  return (readMaybe tmp :: Maybe Int)


trainer :: IO()
trainer = do
  clearFromLine 0
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  putStrLn "t Test Mode, f Free Practise"
  mode <- withEcho False getChar
  case mode of
    't' -> do
      putStrLn "q Quick (10 questions), m Medium (25), l Long (50)"
      len <- takeLen
      putStrLn "e Easy, m Medium, h Hard"
      difficulty <- takeDiff
      intervalTest len difficulty
    'f' -> freePractise
    _   -> trainer

  where
    takeLen :: IO Int
    takeLen = do
      hFlush stdout
      len <- withEcho False getChar
      case len of
        'q' -> return 10
        'm' -> return 25
        'l' -> return 50
        _   -> takeLen

    takeDiff :: IO [Int]
    takeDiff = do
      hFlush stdout
      diff <- withEcho False getChar
      case diff of
        'e' -> return  [3, 4, 7, 12, -3, -4, -7, -12]
        'm' -> return  [1, 2, 3, 4, 6, 7, 10, 11, 12, -1, -2, -3, -4, -6, -7, -10, -11, -12]
        'h' -> return  []
        _   -> takeDiff
