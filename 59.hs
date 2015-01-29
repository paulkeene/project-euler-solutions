import Control.Applicative
import Data.Bits
import Data.Char
import qualified Data.List as L
import qualified Data.List.Split as L
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import qualified Data.Set as S

type Dictionary = S.Set String
newtype CharCode = CharCode { getCharCode :: Int } deriving (Show, Eq, Ord)
newtype Password = Password { getPassword :: [CharCode] } deriving (Show, Eq,
                                                                    Ord)

charCodes :: [CharCode]
charCodes = map (CharCode . ord) ['a'..'z']

toCharCodes :: String -> [CharCode]
toCharCodes = map (CharCode . ord)

fromCharCodes :: [CharCode] -> String
fromCharCodes = map (chr . getCharCode)

decrypt :: [CharCode] -> Password -> [CharCode]
decrypt cipherText password = clearText
  where
    clearText = map CharCode $ zipWith xor (map getCharCode cipherText)
                (cycle $ map getCharCode $ getPassword password)

potentialPasswords :: [Password]
potentialPasswords = [Password [x, y, z] |
                      x <- charCodes,
                      y <- charCodes,
                      z <- charCodes]

findPassword :: Dictionary -> [CharCode] -> Maybe Password
findPassword dict cipherTextCodes = password
  where
    password = L.find (isPasswordCorrect dict cipherTextCodes)
               potentialPasswords

isPasswordCorrect :: Dictionary -> [CharCode] -> Password -> Bool
isPasswordCorrect dict cipherTextCodes pass = foundRatio >= 0.75
  where
    clearText = map (toLower . chr . getCharCode) $ decrypt cipherTextCodes pass
    wordsInDict = map (`S.member` dict) $ words clearText
    numWordsFound = length $ filter id wordsInDict
    foundRatio = fromIntegral numWordsFound /
                 fromIntegral (length wordsInDict)

solution :: Dictionary -> [CharCode] -> Maybe Int
solution dict cipherTextCodes = soln
  where
    password = findPassword dict cipherTextCodes
    soln = sum <$> map getCharCode <$> decrypt cipherTextCodes <$> password

getCipherTextCodes :: IO [CharCode]
getCipherTextCodes = map (CharCode . read) <$> L.splitOn "," <$>
                     readFile "input_files/59_cipher.txt"

getDictionary :: IO (S.Set String)
getDictionary = S.fromList <$> map (map toLower) <$> lines <$>
                  readFile "input_files/59_google_10000_english.txt"

main :: IO ()
main = do
  cipherTextCodes <- getCipherTextCodes
  dictionary <- getDictionary
  print $ solution dictionary cipherTextCodes
