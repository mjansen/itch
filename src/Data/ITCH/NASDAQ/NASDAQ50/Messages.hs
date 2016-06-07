module Data.ITCH.NASDAQ.NASDAQ50.Messages where

import Data.Word
import qualified Data.Binary           as S
import qualified Data.Binary.Get       as S
import qualified Data.Binary.Put       as S
import qualified Data.ByteString.Char8 as BC

newtype TimeStamp6 = T6 Word64
                   deriving (Eq, Show)

instance S.Binary TimeStamp6 where
  get = do
    a <- fromIntegral <$> S.getWord16be
    b <- fromIntegral <$> S.getWord32be
    return . T6 $ (a*(2^32)) + b
  put (T6 x) = S.putWord16be (fromIntegral $ x `quot` (2^32)) >> S.putWord32be (fromIntegral $ x `mod` (2^32))

data SystemEventMessage = SystemEventMessage
  { sem_stockLocate    :: Word16
  , sem_trackingNumber :: Word16
  , sem_timestamp      :: TimeStamp6
  , sem_eventCode      :: Char
  } deriving (Eq, Show)

instance S.Binary SystemEventMessage where
  get = do
    'S' <- S.get
    SystemEventMessage <$> S.getWord16be <*> S.getWord16be <*> S.get <*> S.get
  put (SystemEventMessage a b c d) = S.put 'S' >> S.put a >> S.put b >> S.put c >> S.put d
  
