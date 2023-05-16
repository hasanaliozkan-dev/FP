import System.IO

-- Compute the nth hexadecimal digit of pi using the BBP formula
bbpHexDigit :: Integer -> Integer
bbpHexDigit n = let s k j = div (16^j) (8*k + j)
                    t k j = mod (s k j) (16^j)
                    u k = mod (4^k) (8*k + 1)
                    f k = div (t k 1 - t k 4 - t k 5 - t k 6) (8*k + 1)
                in mod (sum [f k * 16^(-k) | k <- [0..n]]) 16

-- Compute pi to n digits using the BBP formula
computePi :: Integer -> Integer -> String
computePi n m = let digits = [0..n+m-1]
                    terms k = div (2^20 * k + 2^8) (k * 2^10 + 512)
                in "3." ++ (concatMap (show . bbpHexDigit . terms) digits)

-- Write pi to a file
writePiToFile :: FilePath -> Integer -> IO ()
writePiToFile filePath n = writeFile filePath (computePi n 1)

main :: IO ()
main = writePiToFile "pi.txt" 1000000000