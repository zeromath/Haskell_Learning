-- https://www.codewars.com/kata/525f50e3b73515a6db000b83
module CreatePhoneNumber where
import Data.List.Split

createPhoneNumber :: [Int] -> String
createPhoneNumber xs = "(" ++ (printList areaCode) ++ ")" ++ " " ++ (printList firstThree) ++ "-" ++ (printList lastFour)
  where [areaCode, firstThree, nextThree, lastDigit] = chunksOf 3 xs
        lastFour = nextThree ++ lastDigit
        printList = concat . map show
