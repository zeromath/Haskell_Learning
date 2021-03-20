pythagoreanTriple :: Int -> [Int]
pythagoreanTriple x = [[a, b, c]| c <- [1..x], b <- [1..c], a <- [1..b], a + b + c == x, a^2 + b^2 == c^2] !! 0

main :: IO()
main = do
    let ans = pythagoreanTriple 1000
    print $ product ans -- 31875000