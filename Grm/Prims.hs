{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- The grm grammar generator
-- Copyright 2011-2012, Brett Letner

module Grm.Prims
  ( module Grm.Prims
  , module Debug.Trace
  )
where

import Control.Monad
import Data.Char
import Data.Generics hiding (empty)
import Data.List
import Data.Unique
import Debug.Trace
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath
import System.IO
import System.IO.Unsafe
import System.Process (system)
import Text.PrettyPrint.Leijen

data Terminator = Terminator | Separator deriving (Show, Eq)

data Horiz = Vert | Horiz deriving (Show, Eq)

data Empty = Empty | NonEmpty deriving (Show, Eq)

data Point = Point { beginLoc :: Loc, endLoc :: Loc }
  deriving (Show, Ord, Eq, Data, Typeable)

data Loc = Loc
  { locFilePath :: FilePath
  , locLine :: Int
  , locColumn :: Int
  } deriving (Show, Eq, Ord, Data, Typeable)

class HasMeta t where
  meta :: t a -> a
  -- mapMetaM :: Monad m => (a -> m b) -> t a -> m (t b)

ppString :: String -> Doc
ppString = text . show

ppInteger :: Integer -> Doc
ppInteger = text . show

type Number = String

ppNumber :: Number -> Doc
ppNumber = text

readNumber :: (Num a, Read a) => Number -> a
readNumber ('-':cs) = negate $ readNumber cs
readNumber cs | isBinary cs = fromIntegral $ readBinary cs
readNumber s = read s

readBinary :: String -> Integer
readBinary = foldl' (\b a -> 2*b + f a) 0 . drop 2
  where
    f '0' = 0
    f '1' = 1
    f c = error $ "readBinary:" ++ show c
    
ppChar :: Char -> Doc
ppChar c = text s
  where
  s = case show c of
    '\'':'\\':x:_ | isUpper x -> "'\\" ++ show (ord c) ++ "'" -- control chars
    str -> str

isFloat :: String -> Bool
isFloat s = '.' `elem` s || 'e' `elem` (map toLower s)

isBinary :: String -> Bool
isBinary s = 'b' `elem` map toLower s

isOctal :: String -> Bool
isOctal s = 'o' `elem` map toLower s

ppDouble :: Double -> Doc
ppDouble = text . show

ppList :: (a -> Doc) -> Terminator -> String -> Horiz -> [a] -> Doc
ppList f a s b cs = case b of
  Horiz -> hsep ds
  Vert -> empty <$> indent 2 (vsep ds) <$> empty
  where
    ds = case cs of
      [] -> []
      _ -> if a == Terminator then map g cs else map g (init cs) ++ [f $ last cs]
    g x = if null s then f x else f x <> text s

nubSort :: Ord a => [a] -> [a]
nubSort = nub . sort

ppLident :: String -> Doc
ppLident = text

ppMlcode :: String -> Doc
ppMlcode = text

ppUident :: String -> Doc
ppUident = text

ppUsym :: String -> Doc
ppUsym = text

singleton :: a -> [a]
singleton a = [a]

ppErr :: Loc -> String -> String
ppErr loc s = ppLoc loc ++ ": " ++ s

ppLoc :: Loc -> String
ppLoc loc = locFilePath loc ++ ":" ++ show (locLine loc) ++ ":" ++ show (1 + locColumn loc)

startLoc :: HasMeta m => m Point -> Loc
startLoc = beginLoc . point

stopLoc :: HasMeta m => m Point -> Loc
stopLoc = endLoc . point

noLoc :: Loc
noLoc = Loc "" 0 0

noPoint :: Point
noPoint = Point noLoc noLoc

initLoc :: FilePath -> Loc
initLoc fn = Loc fn 1 0

point :: HasMeta m => m Point -> Point
point = meta

lrPoint :: [Point] -> Point
lrPoint xs = case filter ((/=) noPoint) xs of
  [] -> noPoint
  _ -> Point (minimum $ map beginLoc xs) (maximum $ map endLoc xs)
  
lrPointList :: HasMeta m => [m Point] -> Point
lrPointList = lrPoint . map point

type Uident = String
type Lident = String
type Mlcode = String
type Usym = String

ppShow :: Pretty a => a -> String
ppShow = show . pretty

unreachable :: a
unreachable = panic "unreachable"

unused :: a
unused = panic "unused"

panic :: String -> a
panic s = error $ "internal:" ++ s

lowercase :: String -> String
lowercase "" = ""
lowercase (c:cs) = toLower c : cs

freshNm :: IO String
freshNm = liftM (show . hashUnique) newUnique

commaSep :: [String] -> String
commaSep = concat . intersperse ", "

mySystem :: String -> IO ()
mySystem s = do
  putStrLn s
  ec <- system s
  case ec of
    ExitSuccess -> return ()
    ExitFailure i -> error $ "unable to execute(" ++ show i ++ "):" ++ s

bitsToEncode :: Integer -> Integer
bitsToEncode 0 = 0
bitsToEncode i = ceiling $ logBase 2 (fromIntegral i :: Double)

uId :: a -> String -> String
{-# NOINLINE uId #-}
uId a s = seq a $ unsafePerformIO $ liftM ((++) s . show . hashUnique) newUnique

writeFileBinary :: FilePath -> String -> IO ()
writeFileBinary fn s = withBinaryFile fn WriteMode $ \h -> hPutStrLn h s

findFile :: [FilePath] -> FilePath -> IO (Maybe FilePath)
findFile [] _ = return Nothing
findFile (d:ds) n = do
  let fn = combine d n
  r <- doesFileExist fn
  if r then return (Just fn) else findFile ds n
