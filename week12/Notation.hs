module Notation where

import Data.Time

siri :: IO ()
-- siri = 
--   putStrLn "What is your name?" >>
--   getLine >>= \name ->
--   getZonedTime >>= \now ->
--   putStrLn (name ++ formatTime defaultTimeLocale ", the time is %H:%M" now)
siri = do
  putStrLn "What is your name?"
  name <- getLine
  now <- getZonedTime
  putStrLn (name ++ formatTime defaultTimeLocale ", the time is %H:%M" now)

-- Looks up a key in dictionary and if found, returns its value as a Just, if not found, returns Nothing
mayLookup :: (Eq a) => Maybe a -> [(a, b)] -> Maybe b
-- mayLookup maybekey assocs = do
--   key <- maybekey
--   result <- lookup key assocs
--   return result
mayLookup maybekey assocs = 
  maybekey >>= \key ->
  lookup key assocs >>= \result ->
  return result
  