{-# LANGUAGE ScopedTypeVariables #-}
module Data.ITCH.NASDAQ.NASDAQ50.Messages where

import Data.Word
import qualified Data.Binary           as S
import qualified Data.Binary.Get       as S
import qualified Data.Binary.Put       as S
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Lazy  as L

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
  get = SystemEvent <$> S.getWord16be <*> S.getWord16be <*> S.get <*> S.get
  put (SystemEvent a b c d) = S.put a >> S.put b >> S.put c >> S.put d
  
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

instance S.Binary StockDirectory where
  get = StockDirectory <$> S.getWord16be <*> S.getWord16be <*> S.get <*> (BS.toShort <$> S.getByteString 8)
                       <*> S.get <*> S.get <*> S.getWord32be <*> S.get <*> S.get
                       <*> (BS.toShort <$> S.getByteString 2) <*> S.get <*> S.get <*> S.get <*> S.get
                       <*> S.get <*> S.getWord32be <*> S.get
  put = undefined

data StockTradingAction = StockTradingAction
  { sta_stockLocate                 :: Word16
  , sta_tracking                    :: Word16
  , sta_timestamp                   :: TimeStamp6
  , sta_stock                       :: ShortByteString -- 8
  , sta_tradingState                :: Char
  , sta_reserved                    :: Char
  , str_reason                      :: ShortByteString -- 4
  } deriving (Eq, Ord, Show)

instance S.Binary StockTradingAction where
  get = StockTradingAction <$> S.getWord16be <*> S.getWord16be <*> S.get <*> (BS.toShort <$> S.getByteString 8)
                           <*> S.get <*> S.get <*> (BS.toShort <$> S.getByteString 4)
  put = undefined

data REGSHORestriction = REGSHORestriction
  { rsr_stockLocate                 :: Word16
  , rsr_tracking                    :: Word16
  , rsr_timestamp                   :: TimeStamp6
  , rsr_stock                       :: ShortByteString -- 8
  , rsr_REGSHOAction                :: Char
  } deriving (Eq, Ord, Show)
  
instance S.Binary REGSHORestriction where
  get = REGSHORestriction <$> S.getWord16be <*> S.getWord16be <*> S.get
                          <*> (BS.toShort <$> S.getByteString 8) <*> S.get
  put = undefined

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

instance S.Binary MarketParticipantPosition where
  get = MarketParticipantPosition <$> S.getWord16be <*> S.getWord16be <*> S.get <*> S.getWord32be
                                  <*> (BS.toShort <$> S.getByteString 8) <*> S.get <*> S.get <*> S.get
  put = undefined

--------------------------------------------------------------------------------

data MWCBDeclineLevel = MWCBDeclineLevel
  { mdl_stockLocate                 :: Word16
  , mdl_trackingNumber              :: Word16
  , mdl_timestamp                   :: TimeStamp6
  , mdl_level1                      :: Word64   -- fixed point 8 decimals
  , mdl_level2                      :: Word64   -- fixed point 8 decimals
  , mdl_level3                      :: Word64   -- fixed point 8 decimals
  } deriving (Eq, Ord, Show)

instance S.Binary MWCBDeclineLevel where
  get = MWCBDeclineLevel <$> S.getWord16be <*> S.getWord16be <*> S.get
                         <*> S.getWord64be <*> S.getWord64be <*> S.getWord64be
  put = undefined

--------------------------------------------------------------------------------

data MWCBBreach = MWCBBreach
  { mbr_stockLocate                 :: Word16
  , mbr_trackingNumber              :: Word16
  , mbr_timestamp                   :: TimeStamp6
  , mbr_breachedLevel               :: Char
  } deriving (Eq, Ord, Show)

instance S.Binary MWCBBreach where
  get = MWCBBreach <$> S.getWord16be <*> S.getWord16be <*> S.get <*> S.get
  put = undefined

--------------------------------------------------------------------------------

data IPOQuotingPeriodUpdate = IPOQuotingPeriodUpdate
  { qpu_stockLocate                 :: Word16
  , qpu_trackingNumber              :: Word16
  , qpu_timestamp                   :: TimeStamp6
  , qpu_stock                       :: ShortByteString -- 8
  , qpu_quotationReleaseTime        :: Word32
  , qpu_quotationReleaseQualifier   :: Char
  , qpu_price                       :: Word32 -- 4 decimal places
  } deriving (Eq, Ord, Show)

instance S.Binary IPOQuotingPeriodUpdate where
  get = IPOQuotingPeriodUpdate <$> S.getWord16be <*> S.getWord16be <*> S.get <*> (BS.toShort <$> S.getByteString 8)
                               <*> S.getWord32be <*> S.get <*> S.getWord32be
  put = undefined

--------------------------------------------------------------------------------

data Other = Other
  { oth_content     :: ShortByteString
  } deriving (Eq, Ord, Show)

instance S.Binary Other where
  get = Other . BS.toShort . L.toStrict <$> S.getRemainingLazyByteString
  put = undefined

data Message = MSystemEvent SystemEvent
             | MStockDirectory StockDirectory
             | MStockTradingAction StockTradingAction
             | MREGSHORestriction REGSHORestriction
             | MMarketParticipantPosition MarketParticipantPosition
             | MMWCBDeclineLevel MWCBDeclineLevel
             | MMWCBBreach MWCBBreach
             | MIPOQuotingPeriodUpdate IPOQuotingPeriodUpdate
             | MOther Char Other
             deriving (Eq, Ord, Show)

instance S.Binary Message where
  get = do
    (c :: Char) <- S.get
    case c of
      'S' -> MSystemEvent               <$> S.get
      'R' -> MStockDirectory            <$> S.get
      'H' -> MStockTradingAction        <$> S.get
      'Y' -> MREGSHORestriction         <$> S.get
      'L' -> MMarketParticipantPosition <$> S.get
      'V' -> MMWCBDeclineLevel          <$> S.get
      'W' -> MMWCBBreach                <$> S.get
      'K' -> MIPOQuotingPeriodUpdate    <$> S.get
      _   -> MOther c                   <$> S.get
  put = undefined

messageType :: Message -> Char               
messageType (MSystemEvent               _) = 'S'
messageType (MStockDirectory            _) = 'R'
messageType (MStockTradingAction        _) = 'H'
messageType (MREGSHORestriction         _) = 'Y'
messageType (MMarketParticipantPosition _) = 'L'
messageType (MMWCBDeclineLevel          _) = 'V'
messageType (MMWCBBreach                _) = 'W'
messageType (MIPOQuotingPeriodUpdate    _) = 'K'
