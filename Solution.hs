{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Solution where
import Control.Applicative
import Control.Monad
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Word8
import Data.Maybe
import Data.List
import Data.Word
import Text.Printf
import System.IO

-- Take a word from the dictionary file and clean it up
sanitiseWord :: B.ByteString -> [Word8]
sanitiseWord = B.unpack . B.map toLower . B.filter isAlpha

-- Take a number read as input, strip all nondigits and turn the chars into ints
sanitiseNumber :: B.ByteString -> [Word8]
sanitiseNumber = B.unpack . B.filter isDigit

wordToNumber :: B.ByteString -> [Word8]
wordToNumber = map encode . sanitiseWord
  where encode x
          | x `elem` [_e]         = 0
          | x `elem` [_j, _n, _q] = 1
          | x `elem` [_r, _w, _x] = 2
          | x `elem` [_d, _s, _y] = 3
          | x `elem` [_f, _t]     = 4
          | x `elem` [_a, _m]     = 5
          | x `elem` [_c, _i, _v] = 6
          | x `elem` [_b, _k, _u] = 7
          | x `elem` [_l, _o, _p] = 8
          | x `elem` [_g, _h, _z] = 9

-- https://bigonotetaking.wordpress.com/2015/11/06/a-trie-in-haskell/
data Trie = Trie { wordsAtNode :: !([B.ByteString])
                 , subtries    :: M.Map Word8 Trie
                 } deriving (Eq, Show)

emptyTrie = Trie [] mempty

insertWord :: Trie -> B.ByteString -> Trie
insertWord t !w = go t (wordToNumber w)
  where
    go (Trie ws m) [] = Trie (w:ws) m
    go (Trie ws m) (x:xs) = Trie ws (M.alter insertRest x m)
      where
        insertRest = Just . flip go xs . fromMaybe emptyTrie

-- All possible encodings
solve1 :: [Word8] -> Trie -> Maybe [[B.ByteString]]
solve1 num dict = go num dict where
  go []     (Trie ws m) = Just [ws]
  go (i:is) (trie@(Trie ws m))
    | trie == emptyTrie = Nothing
    | M.null m          = Nothing
    | otherwise         = do
        rs       <- solve1 (i:is) dict
        subtries <- M.lookup i m
        recur    <- go is subtries
        return $ (do { r <- rs; w <- ws; return (w:r) }) ++ recur

main :: IO ()
main = do
  dictW   <- B8.lines <$> B.readFile "dictionary.txt"
  numbers <- B8.lines <$> B.readFile "input.txt"
  out     <- openFile  "mySolution.txt" WriteMode
  let dict = foldl' insertWord emptyTrie dictW
--  print dict
{-
38-: so
-885/63538: Opa 6 da so
-885/63538: O"l Midas 8
-885/63538: Po Midas 8
-}
  print $ wordsAtNode (subtries (subtries dict M.! 3) M.! 8)
  print (solve1 (sanitiseNumber "38-") dict) -- expected "so"
  -- print (solve1 (sanitiseNumber "-885/63538") dict)

  -- forM_ numbers $ \n -> do
  --   let sols = solve1 (sanitiseNumber n) dict
  --   forM_ sols $ \sol ->
  --     mapM_ (hprintf out "%s: %s" n) sol
  -- hClose out
  return ()

{- TIME SPENT:
~1hr feb 19 2017 (incomplete)
-}
