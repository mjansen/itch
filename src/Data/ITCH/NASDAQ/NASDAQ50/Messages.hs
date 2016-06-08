module Data.ITCH.NASDAQ.NASDAQ50.Messages where

import Data.Word
import qualified Data.Binary           as S
import qualified Data.Binary.Get       as S
import qualified Data.Binary.Put       as S
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Short as BS

type ShortByteString = BS.ShortByteString

newtype TimeStamp6 = T6 Word64
                   deriving (Eq, Ord, Show)

instance S.Binary TimeStamp6 where
  get = do
    a <- fromIntegral <$> S.getWord16be
    b <- fromIntegral <$> S.getWord32be
    return . T6 $ (a*(2^32)) + b
  put (T6 x) = S.putWord16be (fromIntegral $ x `quot` (2^32)) >> S.putWord32be (fromIntegral $ x `mod` (2^32))

data SystemEvent = SystemEvent
  { sem_stockLocate    :: Word16
  , sem_trackingNumber :: Word16
  , sem_timestamp      :: TimeStamp6
  , sem_eventCode      :: Char
  } deriving (Eq, Ord, Show)

instance S.Binary SystemEvent where
  get = do
    'S' <- S.get
    SystemEvent <$> S.getWord16be <*> S.getWord16be <*> S.get <*> S.get
  put (SystemEvent a b c d) = S.put 'S' >> S.put a >> S.put b >> S.put c >> S.put d
  
data StockDirectory = StockDirectory
  { sdm_stockLocate                 :: Word16
  , sdm_tracking                    :: Word16
  , sdm_timestamp                   :: TimeStamp6
  , sdm_stock                       :: ShortByteString -- 8
  , sdm_marketCategory              :: Char
  , sdm_financialStatusIndicator    :: Char
  , sdm_roundLotSize                :: Word32
  , sdm_roundLotsOnly               :: Char
  , sdm_issueClassification         :: Char
  , sdm_issueSubType                :: ShortByteString -- 2
  , sdm_authenticity                :: Char
  , sdm_shortSaleThresholdIndicator :: Char
  , sdm_IPOFlag                     :: Char
  , sdm_LULDReferencePriceTier      :: Char
  , sdm_ETPFlag                     :: Char
  , sdm_ETPLeverageFactor           :: Word32
  , sdm_inverseIndicator            :: Char
  } deriving (Eq, Ord, Show)

data StockTradingAction = StockTradingAction
  { sta_stockLocate                 :: Word16
  , sta_tracking                    :: Word16
  , sta_timestamp                   :: TimeStamp6
  , sta_stock                       :: ShortByteString -- 8
  , sta_tradingState                :: Char
  , sta_reserved                    :: Char
  , str_reason                      :: ShortByteString
  } deriving (Eq, Ord, Show)

data REGSHORestriction = REGSHORestriction
  { rsr_stockLocate                 :: Word16
  , rsr_tracking                    :: Word16
  , rsr_timestamp                   :: TimeStamp6
  , rsr_stock                       :: ShortByteString -- 8
  , rsr_REGSHOAction                :: Char
  } deriving (Eq, Ord, Show)
  
data MarketParticipantPosition = MarketParticipantPosition
  { mpp_stockLocate                 :: Word16
  , mpp_tracking                    :: Word16
  , mpp_timestamp                   :: TimeStamp6
  , mpp_mpid                        :: Word32
  , mpp_stock                       :: ShortByteString -- 8
  , mpp_primaryMarketMaker          :: Char
  , mpp_marketMakerMode             :: Char
  , mpp_marketPartcipant            :: Char
  } deriving (Eq, Ord, Show)

data Message = MSystemEvent SystemEvent
             | MStockDirectory StockDirectory
             | MStockTradingAction StockTradingAction
             | MREGSHORestriction REGSHORestriction
             deriving (Eq, Ord, Show)

messageType :: Message -> Char               
messageType (MSystemEvent        _) = 'S'
messageType (MStockDirectory     _) = 'D'
messageType (MStockTradingAction _) = 'H'
messageType (MREGSHORestriction  _) = 'Y'
