-- https://www.codewars.com/kata/5263c6999e0f40dee200059d
module PIN where

import Control.Applicative

getPINs :: String -> [String]
getPINs = foldl (flip (liftA2 (:))) [""] . map getNearby . reverse
    
getNearby :: Char -> String
getNearby '1' = "124"
getNearby '2' = "1235"
getNearby '3' = "236"
getNearby '4' = "1457"
getNearby '5' = "24568"
getNearby '6' = "3569"
getNearby '7' = "478"
getNearby '8' = "57890"
getNearby '9' = "689"
getNearby '0' = "08"
getNearby _ = "undefined"
