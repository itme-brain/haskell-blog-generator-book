{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
module Markup
  ( Document,
    Structure (..),
  )
where

import Numeric.Natural

type Document = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Show)

replicate' :: Int -> a -> [a]
replicate' n x =
  if n <= 0
    then []
    else x : replicate' (n - 1) x

even' :: Int -> Bool
even' n =
  if n == 0
    then True
    else odd' (n - 1)

odd' :: Int -> Bool
odd' n =
  if n == 0
    then False
    else even' (n - 1)

parse :: String -> Document
parse = parseLines [] . lines -- (1)

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph txts =
  let paragraph = Paragraph (unlines (reverse currentParagraph)) -- (2), (3)
   in case txts of -- (4)
        [] -> [paragraph]
        currentLine : rest ->
          if trim currentLine == ""
            then paragraph : parseLines [] rest -- (5)
            else parseLines (currentLine : currentParagraph) rest -- (6)

trim :: String -> String
trim = unwords . words
