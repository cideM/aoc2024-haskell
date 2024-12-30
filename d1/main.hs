{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Wextra -Wno-unused-top-binds #-}

import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as TR

distance :: Int -> Int -> Int
distance x y = abs $ x - y

decimal :: Text -> Either Text Int
decimal s = case TR.decimal s of
  Left err -> Left (T.pack err)
  Right (v, "") -> Right v
  Right (_, remainder) -> Left ("Text remaining: " <> remainder)

parseLine :: Text -> Either Text (Int, Int)
parseLine s = case T.words s of
  [a, b] -> do
    a' <- decimal a
    b' <- decimal b
    return (a', b')
  _ -> Left ("Couldn't parse line: " <> s)

part2 :: [Int] -> [Int] -> Int
part2 left right = sum [(n * (lookUpCount n)) | n <- left]
  where
    lookUpCount :: Int -> Int -- how often was `x` in `countsRight`?
    lookUpCount x = fromMaybe 0 (x `lookup` countsRight)
    countsRight :: [(Int, Int)] -- (number, how often it was found)
    countsRight = [(x, 1 + (length xs)) | (x :| xs) <- (NE.group right)]

solve :: Text -> Text
solve s =
  case traverse parseLine (T.lines s) of
    Left err -> error (T.unpack err)
    Right (pairs :: [(Int, Int)]) ->
      let left = sort [a | (a, _) <- pairs]
          right = sort [b | (_, b) <- pairs]
          p1 = sum (zipWith distance left right)
       in T.show (p1, (part2 left right))

main :: IO ()
main = T.interact solve
