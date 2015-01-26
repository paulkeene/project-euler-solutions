import qualified Data.Set as S

powerfulIntsWithBase :: Integer -> [Integer]
powerfulIntsWithBase n = map fst $ filter (lenOpExp (==)) ps
  where
    ps :: [(Integer, Integer)]
    ps = takeWhile (lenOpExp (>=)) $ powers n
    lenOpExp op (v, e) = fromIntegral (length $ show v) `op` e

powers :: Integer -> [(Integer, Integer)]
powers n = map (f n) [1..]
  where
    f :: Integer -> Integer -> (Integer, Integer)
    f n e = (n ^ e, e)

nDigitIntegers :: S.Set Integer
nDigitIntegers = foldl f S.empty [1..9]
  where
    f :: S.Set Integer -> Integer -> S.Set Integer
    f s i = S.union s $ S.fromList (powerfulIntsWithBase i)

main :: IO ()
main = do
  print . S.toList $ soln
  print . S.size $ soln
  where
    soln = nDigitIntegers
