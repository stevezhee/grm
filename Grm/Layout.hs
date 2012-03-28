{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- The grm grammar generator
-- Copyright 2011-2012, Brett Letner

module Grm.Layout (layout) where

import Grm.Lex
import Grm.Prims

layout :: [Token Point] -> [Token Point]
layout ts = loop [] ts
  where
  loop [] [] = []

  loop (_:cs) [] = stopSyms eofLoc ++ loop cs []

  loop cs (x:y:xs) | isStopSym x && isStopWord y = x : y : loop cs xs

  loop (_:cs) (x:xs) | isStopWord x =
    stopSyms (startLoc x) ++ [x] ++ loop (filter ((>=) $ col x) cs) xs

  loop (c:cs) xs0@(x:_) | col x < c = stopSyms (startLoc x) ++ loop cs xs0

  loop (c:cs) (x:xs) | col x == c && (isWord ["where"] x) = stopSyms (startLoc x) ++ cont cs x xs

  loop cs0@(c:_) (x:xs) | col x == c = termSym (startLoc x) : cont cs0 x xs

  loop cs0 (x:xs) = cont cs0 x xs

  cont cs0 x0 [] | isStartWord x0 = x0 : startSym (stopLoc x0) : loop (col x0 : cs0) []

  cont cs0 x0 (x:xs) | isStartWord x0 && not (isStartSym x) =
    x0 : startSym (startLoc x) : cont (col x : cs0) x xs

  cont cs0 x xs0 = x : loop cs0 xs0

  eofLoc = stopLoc $ last ts

isStartWord :: Token Point -> Bool
isStartWord = isWord [ "where", "do", "of", "imports", "exports", "branch" ] -- , "let"

isStopWord :: Token Point -> Bool
isStopWord = isWord [ "until" ]

isStartSym :: Token Point -> Bool
isStartSym = isWord ["{"]

isStopSym :: Token Point -> Bool
isStopSym = isWord ["}"]

isWord :: [String] -> Token Point -> Bool
isWord ss (TSymbol _ s) = s `elem` ss
isWord _ _ = False

col :: Token Point -> Int
col = locColumn . startLoc

mkSym :: String -> Loc -> Token Point
mkSym s l = TSymbol (Point l l) s

startSym :: Loc -> Token Point
startSym = mkSym "{"

stopSyms :: Loc -> [Token Point]
stopSyms loc = [ termSym loc, mkSym "}" loc ]

termSym :: Loc -> Token Point
termSym = mkSym ";"
