{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Wextra -Wno-unused-top-binds #-}

-- Included libraries:
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/9.12.1-notes.html#included-libraries

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as TR

parseLine :: Text -> Either Text (Int, Int)
parseLine s = case T.words s of
  [a, b] -> do
    a' <- decimal a
    b' <- decimal b
    return (a', b')
  _ -> Left ("Couldn't parse line: " <> s)

solve :: Text -> Text
solve s =
  case traverse parseLine (T.lines s) of
    Left err -> error (T.unpack err)
    Right (_ :: [Text]) -> "TODO"

main :: IO ()
main = T.interact solve
