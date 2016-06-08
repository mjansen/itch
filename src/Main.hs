module Main where

import qualified Data.Binary            as S
import qualified Data.Binary.Get        as S
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Lazy   as L
import qualified Codec.Compression.GZip as GZ

import System.Environment

import qualified Data.ITCH.NASDAQ.NASDAQ50.Messages as N50

testfile1 = "/home/mjansen/ASX/itch/nasdaq/01302016.NASDAQ_ITCH50.gz"
testfile2 = "/home/mjansen/ASX/itch/nasdaq/05272016.NASDAQ_ITCH50.gz"

data Message = Message
  { m_length  :: Int
  , m_content :: L.ByteString
  } deriving (Eq, Ord, Show)

instance S.Binary Message where
  get = do
    len <- fromIntegral <$> S.getWord16be
    str <- S.getLazyByteString (fromIntegral len)
    return $ Message len str
  put = undefined

decodeToList :: L.ByteString -> [Message]
decodeToList str =
  if L.null str
  then []
  else let m = S.decode str :: Message
           ms = decodeToList (L.drop (2 + (fromIntegral $ m_length m)) str)
       in m:ms

main :: IO ()
main = do
  [testfile] <- getArgs
  rs <- decodeToList . GZ.decompress <$> L.readFile testfile
  mapM_ printMessage rs
  -- let feature = BC.head . m_content
  -- putStr . map feature $ rs

printMessage :: Message -> IO ()
printMessage msg@(Message len con) = do
  -- print msg
  case S.decode con :: N50.Message of
    N50.MOther _ _ -> return ()
    m              -> print m
    
