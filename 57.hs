import Data.Digits
import Data.Ratio

expansions :: Int -> Rational
expansions n = 1 + expansionsHelper n

expansionsHelper :: Int -> Rational
expansionsHelper 0 = 0
expansionsHelper n = 1 / (2 + expansionsHelper (n - 1))

solution :: Int
solution = length . filter f $ map expansions [1..1000]
  where
    f n = g (numerator n) > g (denominator n)
    g = length . digits 10

main :: IO ()
main = print solution
