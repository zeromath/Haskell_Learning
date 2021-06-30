-- https://www.codewars.com/kata/5279f6fe5ab7f447890006a7
module PickPeak.JorgeVS.Kata where
data PickedPeaks = PickedPeaks { pos :: [Int], peaks :: [Int]} deriving (Eq, Show)
pickPeaks :: [Int] -> PickedPeaks
pickPeaks d = PickedPeaks (fst <$> pos_data) (snd <$> pos_data)
  where
    filtered_data = foldl removeConsectiveDup [] $ zip [0..] d
    shift_by_1 = tail filtered_data
    shift_by_2 = tail shift_by_1
    peak x y z = if snd y > snd x && snd y > snd z
                   then y
                   else (-1, -1)
    pos_data = filter (/=(-1, -1)) $ zipWith3 peak filtered_data shift_by_1 shift_by_2

removeConsectiveDup :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
removeConsectiveDup [] x = [x]
removeConsectiveDup ys x
  | snd x == snd (last ys) = ys
  | otherwise = ys ++ [x]
