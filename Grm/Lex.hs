{-# OPTIONS -Wall #-}

-- The grm grammar generator
-- Copyright 2011-2012, Brett Letner

module Grm.Lex
  ( Token(..)
  , Point(..)
  , lexFilePath
  , lexContents
  , ppToken
  , ppTokenList
  , happyError
  , notWSToken
  , unTWhitespace
  , unTSLComment
  , unTMLComment
  , unTString
  , unTChar
  , unTNumber
  , unTSymbol
  , unTUsym
  , unTUident
  , unTLident
  ) where

import Control.Monad
import Data.Char
-- import Numeric
import Data.List
import Grm.Prims
import Text.PrettyPrint.Leijen

happyError :: [Token Point] -> a
happyError [] = error "unexpected end of tokens"
happyError (t:_) = error $ ppErr (startLoc t) ("unexpected token: " ++ show (show (ppToken t)))

-- fixme: doesn't work for empty modules (e.g. module Foo where)

notWSToken :: Token t -> Bool
notWSToken t = case t of
  TWhitespace{} -> False
  TSLComment{} -> False
  TMLComment{} -> False
  _ -> True

data Token a
  = TWhitespace a String
  | TSLComment a String
  | TMLComment a String
  | TString a String
  | TChar a Char
  | TNumber a String
  | TSymbol a String
  | TUsym a String
  | TUident a String
  | TLident a String
  deriving (Show)

unTWhitespace :: Token t -> String
unTWhitespace (TWhitespace _ b) = b
unTWhitespace _ = error "unTWhitespace"

unTSLComment :: Token t -> String
unTSLComment (TSLComment _ b) = b
unTSLComment _ = error "unTSLComment"

unTMLComment :: Token t -> String
unTMLComment (TMLComment _ b) = b
unTMLComment _ = error "unTMLComment"

unTString :: Token t -> String
unTString (TString _ b) = b
unTString _ = error "unTString"

unTChar :: Token t -> Char
unTChar (TChar _ b) = b
unTChar _ = error "unTChar"

unTNumber :: Token t -> String
unTNumber (TNumber _ b) = b
unTNumber _ = error "unTNumber"

unTSymbol :: Token t -> String
unTSymbol (TSymbol _ b) = b
unTSymbol _ = error "unTSymbol"

unTUsym :: Token t -> String
unTUsym (TUsym _ b) = b
unTUsym _ = error "unTUsym"

unTUident :: Token t -> String
unTUident (TUident _ b) = b
unTUident _ = error "unTUident"

unTLident :: Token t -> String
unTLident (TLident _ b) = b
unTLident _ = error "unTLident"

instance Eq (Token a) where
  (==) a b = case (a,b) of
    (TWhitespace _ x, TWhitespace _ y) -> x == y
    (TSLComment _ x, TSLComment _ y) -> x == y
    (TMLComment _ x, TMLComment _ y) -> x == y
    (TString _ x, TString _ y) -> x == y
    (TChar _ x, TChar _ y) -> x == y
    (TNumber _ x, TNumber _ y) -> x == y -- numbers are compared lexically
    (TSymbol _ x, TSymbol _ y) -> x == y
    (TUsym _ x, TUsym _ y) -> x == y
    (TUident _ x, TUident _ y) -> x == y
    (TLident _ x, TLident _ y) -> x == y
    _ -> False

ppTokenList :: [Token a] -> Doc
ppTokenList = sep . map ppToken

ppToken :: Token a -> Doc
ppToken t = text $ case t of
  TWhitespace _ s -> s
  TSLComment _ s -> s
  TMLComment _ s -> s
  TString _ a -> show a
  TChar _ a -> show a
  TNumber _ s -> s
  TSymbol _ s -> s
  TUsym _ s -> s
  TUident _ s -> s
  TLident _ s -> s

instance HasMeta Token where
  meta t = case t of
    TWhitespace a _ -> a
    TSLComment a _ -> a
    TMLComment a _ -> a
    TString a _ -> a
    TChar a _ -> a
    TNumber a _ -> a
    TSymbol a _ -> a
    TUsym a _ -> a
    TUident a _ -> a
    TLident a _ -> a

  -- mapMetaM f t = case t of
  --   TWhitespace a b -> f a >>= \x -> return (TWhitespace x b)
  --   TSLComment a b -> f a >>= \x -> return (TSLComment x b)
  --   TMLComment a b -> f a >>= \x -> return (TMLComment x b)
  --   TString a b -> f a >>= \x -> return (TString x b)
  --   TChar a b -> f a >>= \x -> return (TChar x b)
  --   TNumber a b -> f a >>= \x -> return (TNumber x b)
  --   TSymbol a b -> f a >>= \x -> return (TSymbol x b)
  --   TUsym a b -> f a >>= \x -> return (TUsym x b)
  --   TUident a b -> f a >>= \x -> return (TUident x b)
  --   TLident a b -> f a >>= \x -> return (TLident x b)

lexFilePath :: [String] -> FilePath -> IO [Token Point]
lexFilePath syms fn = do
  s <- readFile fn
  lexContents syms fn s

lexContents :: [String] -> FilePath -> String -> IO [Token Point]
lexContents syms fn s = do
  let st0 = initSt syms fn $ filter ((/=) '\r') s
  let M f = lexTokens
  case f st0 of
    (Left e, st) -> error $ ppErr (stLoc st) e
    (Right ts, st) -> case stInput st of
      "" -> return ts
      e -> error $ ppErr (stLoc st) $ "lexical error:" ++ show (head e)

initSt :: [String] -> FilePath -> String -> St
initSt syms fn s = St
  { stLoc = initLoc fn
  , stInput = s
  , stSymbols = syms
  }

instance Monad M where
  (M f) >>= g = M $ \st ->
    let (ma, st1) = f st
      in case ma of
        Left e -> (Left e, st)
        Right a ->
          let M h = g a
            in h st1

  return a = M $ \st -> (Right a, st)
  fail s = M $ \st -> (Left s, st)

data M a = M (St -> (Either String a, St))

getSt :: M St
getSt = M $ \st -> (Right st, st)

lexAnyChar :: M Char
lexAnyChar = M $ \st ->
  let
    loc0 = stLoc st
    in case stInput st of
      "" -> (Left "unexpected eof", st)
      (c:cs) | c == eolChar ->
        (Right c, st{ stInput = cs
                    , stLoc = loc0{ locColumn = 0, locLine = succ (locLine loc0) }})
      (c:cs) ->
        (Right c, st{ stInput = cs
                    , stLoc = loc0{ locColumn = succ (locColumn loc0) }})

tryLex :: M a -> M (Maybe a)
tryLex (M f) = M $ \st -> case f st of
  (Left _, _) -> (Right Nothing, st)
  (Right a, st1) -> (Right $ Just a, st1)

data St = St
  { stLoc :: Loc
  , stInput :: String
  , stSymbols :: [String]
  } deriving (Show)

qualChar :: Char
qualChar = '.'

escChar :: Char
escChar = '\\'

dQuoteChar :: Char
dQuoteChar = '"'

sQuoteChar :: Char
sQuoteChar = '\''

eolChar :: Char
eolChar = '\n'

wsChar :: String
wsChar = [ ' ', eolChar ]

slCommentStart :: String
slCommentStart = "//"

mlCommentStart :: String
mlCommentStart = "/*"

mlCommentEnd :: String
mlCommentEnd = "*/"

-- slCommentStart = "--", doesn't work if you support the decrement statement, e.g. i--
-- mlCommentStart = "{-"
-- mlCommentEnd = "-}"

uidentStart :: String
uidentStart = [ 'A' .. 'Z' ]

lidentStart :: String
lidentStart = '_' : [ 'a' .. 'z' ]

identEnd :: String
identEnd = uidentStart ++ lidentStart ++ [ '0' .. '9' ]

getLoc :: M Loc
getLoc = liftM stLoc getSt

getSymbols :: M [String]
getSymbols = liftM stSymbols getSt

lexStringLit :: String -> M String
lexStringLit s = do
  s1 <- sequence $ replicate (length s) lexAnyChar
  if s1 == s
    then return s
    else fail $ "unexpected string:" ++ s

lexCharPred :: (Char -> Bool) -> M Char
lexCharPred p = do
  c <- lexAnyChar
  if p c
    then return c
    else fail $ "unexpected char:" ++ show c

many :: M a -> M [a]
many m = do
  mx <- tryLex m
  case mx of
    Just x -> do
      xs <- many m
      return $ x : xs
    Nothing -> return []

oneof :: [M a] -> M a
oneof [] = fail "token doesn't match any alternatives"
oneof (m:ms) = do
  mx <- tryLex m
  case mx of
    Just x -> return x
    Nothing -> oneof ms

lexKeyword :: M (Token Point)
lexKeyword = do
  a <- getLoc
  s <- oneof [ lexUpperWord, lexLowerWord ]
  ss <- getSymbols
  b <- getLoc
  if s `elem` ss
    then return $ TSymbol (Point a b) s
    else fail $ "not a keyword:" ++ s

lexSymbol :: M (Token Point)
lexSymbol = do
  a <- getLoc
  ss <- getSymbols
  s <- oneof [ lexUsymTok, liftM singleton $ lexCharPred (flip elem symChar) ]
  b <- getLoc
  if s `elem` ss
    then return $ TSymbol (Point a b) s
    else fail $ "not a symbol:" ++ s

usymChar :: String
usymChar = "!#$%&*+-/:<=>?@\\^|~"

symChar :: String
symChar = "(),;`{}[]."

lexUsymTok :: M String
lexUsymTok = many1 (lexCharPred $ flip elem usymChar)

lexUsym :: M (Token Point)
lexUsym = do
  a <- getLoc
  s <- lexUsymTok
  b <- getLoc
  return $ TUsym (Point a b) s

lexUident :: M (Token Point)
lexUident = do
  a <- getLoc
  s <- lexQualified
  b <- getLoc
  return $ TUident (Point a b) s

lexLident :: M (Token Point)
lexLident = do
  a <- getLoc
  s <- oneof [ lexLowerQualified, lexLowerWord ]
  b <- getLoc
  return $ TLident (Point a b) s

lexLowerQualified :: M String
lexLowerQualified = do
    s1 <- lexQualified
    s2 <- do
      _ <- lexCharPred ((==) qualChar)
      lexLowerWord
    return $ qualify [s1,s2]

lexQualified :: M String
lexQualified = do
  s0 <- lexUpperWord
  ss <- many $ do
    _ <- lexCharPred ((==) qualChar)
    s <- lexUpperWord
    return s
  return $ qualify $ s0 : ss

lexUpperWord :: M String
lexUpperWord = do
  c <- lexCharPred (flip elem uidentStart)
  cs <- many $ lexCharPred (flip elem identEnd)
  return $ c : cs

lexLowerWord :: M String
lexLowerWord = do
  c <- lexCharPred (flip elem lidentStart)
  cs <- many $ lexCharPred (flip elem identEnd)
  return $ c : cs

qualify :: [String] -> String
qualify = concat . intersperse [qualChar]

many1 :: M a -> M [a]
many1 m = do
  x <- m
  xs <- many m
  return $ x : xs

lexTokens :: M [Token Point]
lexTokens = many lexToken

lexToken :: M (Token Point)
lexToken = oneof
  [ lexWhitespace
  , lexSingleLineComment
  , lexMultiLineComment
  , lexString
  , lexChar
  , lexNumber
  , lexKeyword
  , lexLident -- must be before lexUident for qualification
  , lexUident
  , lexSymbol
  , lexUsym
  ]

lexWhitespace :: M (Token Point)
lexWhitespace = do
  a <- getLoc
  s <- many1 $ lexCharPred (flip elem wsChar)
  b <- getLoc
  return $ TWhitespace (Point a b) s

lexSingleLineComment :: M (Token Point)
lexSingleLineComment = do
  a <- getLoc
  _ <- lexStringLit slCommentStart
  cs <- many $ lexCharPred ((/=) eolChar)
  b <- getLoc
  return $ TSLComment (Point a b) $ slCommentStart ++ cs

lexMultiLineComment :: M (Token Point)
lexMultiLineComment = do
  a <- getLoc
  s <- lexMLCommentStart
  b <- getLoc
  return $ TMLComment (Point a b) s

lexMLCommentStart :: M String
lexMLCommentStart = do
  sa <- lexStringLit mlCommentStart
  sb <- lexMLCommentEnd
  return $ sa ++ sb

lexMLCommentEnd :: M String
lexMLCommentEnd = do
  ms0 <- tryLex $ lexStringLit mlCommentEnd
  case ms0 of
    Just s -> return s
    Nothing -> do
      ms <- tryLex lexMLCommentStart
      sa <- case ms of
        Just s -> return s
        Nothing -> do
          c <- lexAnyChar
          return [c]
      sb <- lexMLCommentEnd
      return $ sa ++ sb

lexChar :: M (Token Point)
lexChar = do
  a <- getLoc
  _ <- lexCharPred ((==) sQuoteChar)
  s <- oneof [ lexEscChar, liftM singleton $ lexCharPred ((/=) sQuoteChar) ]
  _ <- lexCharPred ((==) sQuoteChar)
  b <- getLoc
  return $ TChar (Point a b) $ read ([sQuoteChar] ++ s ++ [sQuoteChar])

lexString :: M (Token Point)
lexString = do
  a <- getLoc
  _ <- lexCharPred ((==) dQuoteChar)
  s <- liftM concat $ many $ oneof [ lexEscChar, liftM singleton $ lexCharPred ((/=) dQuoteChar) ]
  _ <- lexCharPred ((==) dQuoteChar)
  b <- getLoc
  return $ TString (Point a b) $ read ([dQuoteChar] ++ s ++ [dQuoteChar])

lexDot :: M String
lexDot = liftM singleton $ lexCharPred ((==) '.')

lexSeq :: [M String] -> M String
lexSeq = liftM concat . sequence

lexExponent :: M String
lexExponent = do
  a <- liftM toLower $
       oneof [ lexCharPred ((==) 'e'), lexCharPred ((==) 'E') ]
  b <- oneof [ lexCharPred ((==) '+') >> return ""
             , liftM singleton $ lexCharPred ((==) '-')
             , return ""
             ]
  c <- lexDecimal
  return $ a : b ++ c
  
lexNumber :: M (Token Point)
lexNumber = do
  a <- getLoc
  c <- oneof
       [ liftM singleton $ lexCharPred ((==) '-')
       , return ""
       ]
  s <- oneof
       [ lex0x "0x" isHexit
       , lex0x "0X" isHexit
       , lex0x "0o" isOctit
       , lex0x "0O" isOctit
       , lex0x "0b" isBinit
       , lex0x "0B" isBinit
       , lexFloat
       , lexDecimal
       ]
  b <- getLoc
  return $ TNumber (Point a b) $ c ++ s

lexFloat :: M String
lexFloat = oneof
  [ lexSeq [ lexDecimal, lexDot, lexDecimal, lexExponent ]
  , lexSeq [ lexDecimal, lexDot, lexDecimal ]
  , lexSeq [ lexDecimal, lexExponent ]
  ]

lexDecimal :: M String
lexDecimal = liftM delLeadingZeros $ many1 $ lexCharPred isDigit

lex0x :: String -> (Char -> Bool) -> M String
lex0x s f = do
  cs <- sequence [lexCharPred ((==) c) | c <- s]
  ds <- many1 $ lexCharPred f
  return $ map toLower (cs ++ delLeadingZeros ds)

delLeadingZeros :: String -> String
delLeadingZeros x = case dropWhile ((==) '0') x of
  [] -> "0"
  a -> a
  
isHexit :: Char -> Bool
isHexit c = isDigit c || toLower c `elem` ['a' .. 'f']

isOctit :: Char -> Bool
isOctit c = c `elem` ['0' .. '7']

isBinit :: Char -> Bool
isBinit c = c `elem` ['0','1']

lexEscChar :: M String
lexEscChar = do
  _ <- lexCharPred ((==) escChar)
  s <- oneof
    [ liftM (show . (read :: String -> Int)) lexDecimal
    , liftM singleton lexAnyChar
    ]
  return $ escChar : s
