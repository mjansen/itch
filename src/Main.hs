module Main where

import qualified Data.Binary            as S
import qualified Data.Binary.Get        as S
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Lazy   as L
import qualified Codec.Compression.GZip as GZ

testfile = "/home/mjansen/ASX/itch/nasdaq/01302016.NASDAQ_ITCH50.gz"

data Message = Message
  { m_length  :: Int
  , m_content :: BC.ByteString
  } deriving (Eq, Ord, Show)

instance S.Binary Message where
  get = do
    len <- fromIntegral <$> S.getWord16be
    str <- S.getByteString len
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
  rs <- decodeToList . GZ.decompress <$> L.readFile testfile
  let feature = BC.head . m_content
  putStr . map feature $ rs
