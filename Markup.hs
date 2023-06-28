{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
module Markup
  ( Document,
    Structure (..),
    parse,
  )
where

import Data.Maybe (maybeToList)
import Numeric.Natural

type Document = [Structure]

data Structure
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving (Eq, Show)

data Context
  = CtxHeading Natural String
  | CtxParagraph [String]
  | CtxUnorderedList [String]
  | CtxOrderedList [String]
  | CtxCodeBlock [String]

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
parse = parseLines Nothing . lines -- (1)

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
  case txts of
    [] -> maybeToList context
    ('*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 1 (trim line) : parseLines Nothing rest)
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) ->
          parseLines (Just (UnorderedList (list <> [trim line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (UnorderedList [trim line])) rest)
    ('#' : ' ' : line) : rest ->
      case context of
        Just (OrderedList list) ->
          parseLines (Just (OrderedList (list <> [trim line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (OrderedList [trim line])) rest)
    ('>' : ' ' : line) : rest ->
      case context of
        Just (CodeBlock list) ->
          parseLines (Just (CodeBlock (list <> [trim line]))) rest
        _ ->
          maybe id (:) context (parseLines (Just (CodeBlock [trim line])) rest)
    currentLine : rest ->
      let line = trim currentLine
       in if line == ""
            then maybe id (:) context (parseLines Nothing rest)
            else case context of
              Nothing -> parseLines Nothing rest
              Just structure -> structure : parseLines Nothing rest

trim :: String -> String
trim = unwords . words
