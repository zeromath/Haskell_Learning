-- https://www.codewars.com/kata/54b724efac3d5402db00065e
module Codewars.Kata.DecodeMorse (decodeMorse) where

import Codewars.Kata.DecodeMorse.Preload (morseCodes)

import Data.Map.Strict ((!))

import Data.List.Split

decodeMorse :: String -> String
decodeMorse codes = unwords . filter (/="") . map decode $ list_codes
  where list_codes = splitOn "   " $ codes
        decode code = concat . map (morseCodes !) $ list_code
          where list_code = filter (/="") . splitOn " " $ code
