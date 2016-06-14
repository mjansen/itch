{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.ITCH.NASDAQ.NASDAQ50.Messages where

import Control.DeepSeq

import Data.Word
import qualified Data.Binary           as S
import qualified Data.Binary.Get       as S
import qualified Data.Binary.Put       as S
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Lazy  as L

import GHC.Generics

type ShortByteString = BS.ShortByteString

newtype TimeStamp6 = T6 Word64
                   deriving (Eq, Ord, Show, Generic)

instance NFData TimeStamp6

instance S.Binary TimeStamp6 where
  get = do
    a <- fromIntegral <$> S.getWord16be
    b <- fromIntegral <$> S.getWord32be
    return . T6 $ (a*(2^32)) + b
  put (T6 x) = S.putWord16be (fromIntegral $ x `quot` (2^32)) >> S.putWord32be (fromIntegral $ x `mod` (2^32))

class MessageBasics a where
  stockLocate    :: a -> Word16
  trackingNumber :: a -> Word16
  timestamp      :: a -> TimeStamp6

data SystemEvent = SystemEvent
  { sem_stockLocate    :: {-# UNPACK #-} !Word16
  , sem_trackingNumber :: {-# UNPACK #-} !Word16
  , sem_timestamp      :: {-# UNPACK #-} !TimeStamp6
  , sem_eventCode      :: {-# UNPACK #-} !Char
  } deriving (Eq, Ord, Show)

instance S.Binary SystemEvent where
  get = SystemEvent <$> S.getWord16be <*> S.getWord16be <*> S.get <*> S.get
  put (SystemEvent a b c d) = S.put a >> S.put b >> S.put c >> S.put d

instance MessageBasics SystemEvent where
  stockLocate    = sem_stockLocate
  trackingNumber = sem_trackingNumber
  timestamp      = sem_timestamp
  
data StockDirectory = StockDirectory
  { sdm_stockLocate                 :: {-# UNPACK #-} !Word16
  , sdm_trackingNumber              :: {-# UNPACK #-} !Word16
  , sdm_timestamp                   :: {-# UNPACK #-} !TimeStamp6
  , sdm_stock                       :: {-# UNPACK #-} !ShortByteString -- 8
  , sdm_marketCategory              :: {-# UNPACK #-} !Char
  , sdm_financialStatusIndicator    :: {-# UNPACK #-} !Char
  , sdm_roundLotSize                :: {-# UNPACK #-} !Word32
  , sdm_roundLotsOnly               :: {-# UNPACK #-} !Char
  , sdm_issueClassification         :: {-# UNPACK #-} !Char
  , sdm_issueSubType                :: {-# UNPACK #-} !ShortByteString -- 2
  , sdm_authenticity                :: {-# UNPACK #-} !Char
  , sdm_shortSaleThresholdIndicator :: {-# UNPACK #-} !Char
  , sdm_IPOFlag                     :: {-# UNPACK #-} !Char
  , sdm_LULDReferencePriceTier      :: {-# UNPACK #-} !Char
  , sdm_ETPFlag                     :: {-# UNPACK #-} !Char
  , sdm_ETPLeverageFactor           :: {-# UNPACK #-} !Word32
  , sdm_inverseIndicator            :: {-# UNPACK #-} !Char
  } deriving (Eq, Ord, Show)

instance S.Binary StockDirectory where
  get = StockDirectory <$> S.getWord16be <*> S.getWord16be <*> S.get <*> (BS.toShort <$> S.getByteString 8)
                       <*> S.get <*> S.get <*> S.getWord32be <*> S.get <*> S.get
                       <*> (BS.toShort <$> S.getByteString 2) <*> S.get <*> S.get <*> S.get <*> S.get
                       <*> S.get <*> S.getWord32be <*> S.get
  put = undefined

instance MessageBasics StockDirectory where
  stockLocate    = sdm_stockLocate
  trackingNumber = sdm_trackingNumber
  timestamp      = sdm_timestamp
  
data StockTradingAction = StockTradingAction
  { sta_stockLocate                 :: {-# UNPACK #-} !Word16
  , sta_trackingNumber              :: {-# UNPACK #-} !Word16
  , sta_timestamp                   :: {-# UNPACK #-} !TimeStamp6
  , sta_stock                       :: {-# UNPACK #-} !ShortByteString -- 8
  , sta_tradingState                :: {-# UNPACK #-} !Char
  , sta_reserved                    :: {-# UNPACK #-} !Char
  , sta_reason                      :: {-# UNPACK #-} !ShortByteString -- 4
  } deriving (Eq, Ord, Show)

instance S.Binary StockTradingAction where
  get = StockTradingAction <$> S.getWord16be <*> S.getWord16be <*> S.get <*> (BS.toShort <$> S.getByteString 8)
                           <*> S.get <*> S.get <*> (BS.toShort <$> S.getByteString 4)
  put = undefined

instance MessageBasics StockTradingAction where
  stockLocate    = sta_stockLocate
  trackingNumber = sta_trackingNumber
  timestamp      = sta_timestamp
  
data REGSHORestriction = REGSHORestriction
  { rsr_stockLocate                 :: {-# UNPACK #-} !Word16
  , rsr_trackingNumber              :: {-# UNPACK #-} !Word16
  , rsr_timestamp                   :: {-# UNPACK #-} !TimeStamp6
  , rsr_stock                       :: {-# UNPACK #-} !ShortByteString -- 8
  , rsr_REGSHOAction                :: {-# UNPACK #-} !Char
  } deriving (Eq, Ord, Show)
  
instance S.Binary REGSHORestriction where
  get = REGSHORestriction <$> S.getWord16be <*> S.getWord16be <*> S.get
                          <*> (BS.toShort <$> S.getByteString 8) <*> S.get
  put = undefined

instance MessageBasics REGSHORestriction where
  stockLocate    = rsr_stockLocate
  trackingNumber = rsr_trackingNumber
  timestamp      = rsr_timestamp

data MarketParticipantPosition = MarketParticipantPosition
  { mpp_stockLocate                 :: {-# UNPACK #-} !Word16
  , mpp_trackingNumber              :: {-# UNPACK #-} !Word16
  , mpp_timestamp                   :: {-# UNPACK #-} !TimeStamp6
  , mpp_mpid                        :: {-# UNPACK #-} !Word32
  , mpp_stock                       :: {-# UNPACK #-} !ShortByteString -- 8
  , mpp_primaryMarketMaker          :: {-# UNPACK #-} !Char
  , mpp_marketMakerMode             :: {-# UNPACK #-} !Char
  , mpp_marketPartcipantState       :: {-# UNPACK #-} !Char
  } deriving (Eq, Ord, Show)

instance S.Binary MarketParticipantPosition where
  get = MarketParticipantPosition <$> S.getWord16be <*> S.getWord16be <*> S.get <*> S.getWord32be
                                  <*> (BS.toShort <$> S.getByteString 8) <*> S.get <*> S.get <*> S.get
  put = undefined

instance MessageBasics MarketParticipantPosition where
  stockLocate    = mpp_stockLocate
  trackingNumber = mpp_trackingNumber
  timestamp      = mpp_timestamp

--------------------------------------------------------------------------------

data MWCBDeclineLevel = MWCBDeclineLevel
  { mdl_stockLocate                 :: {-# UNPACK #-} !Word16
  , mdl_trackingNumber              :: {-# UNPACK #-} !Word16
  , mdl_timestamp                   :: {-# UNPACK #-} !TimeStamp6
  , mdl_level1                      :: {-# UNPACK #-} !Word64   -- fixed point 8 decimals
  , mdl_level2                      :: {-# UNPACK #-} !Word64   -- fixed point 8 decimals
  , mdl_level3                      :: {-# UNPACK #-} !Word64   -- fixed point 8 decimals
  } deriving (Eq, Ord, Show)

instance S.Binary MWCBDeclineLevel where
  get = MWCBDeclineLevel <$> S.getWord16be <*> S.getWord16be <*> S.get
                         <*> S.getWord64be <*> S.getWord64be <*> S.getWord64be
  put = undefined

instance MessageBasics MWCBDeclineLevel where
  stockLocate    = mdl_stockLocate
  trackingNumber = mdl_trackingNumber
  timestamp      = mdl_timestamp

--------------------------------------------------------------------------------

data MWCBBreach = MWCBBreach
  { mbr_stockLocate                 :: {-# UNPACK #-} !Word16
  , mbr_trackingNumber              :: {-# UNPACK #-} !Word16
  , mbr_timestamp                   :: {-# UNPACK #-} !TimeStamp6
  , mbr_breachedLevel               :: {-# UNPACK #-} !Char
  } deriving (Eq, Ord, Show)

instance S.Binary MWCBBreach where
  get = MWCBBreach <$> S.getWord16be <*> S.getWord16be <*> S.get <*> S.get
  put = undefined

instance MessageBasics MWCBBreach where
  stockLocate    = mbr_stockLocate
  trackingNumber = mbr_trackingNumber
  timestamp      = mbr_timestamp

--------------------------------------------------------------------------------

data IPOQuotingPeriodUpdate = IPOQuotingPeriodUpdate
  { qpu_stockLocate                 :: {-# UNPACK #-} !Word16
  , qpu_trackingNumber              :: {-# UNPACK #-} !Word16
  , qpu_timestamp                   :: {-# UNPACK #-} !TimeStamp6
  , qpu_stock                       :: {-# UNPACK #-} !ShortByteString -- 8
  , qpu_quotationReleaseTime        :: {-# UNPACK #-} !Word32
  , qpu_quotationReleaseQualifier   :: {-# UNPACK #-} !Char
  , qpu_price                       :: {-# UNPACK #-} !Word32 -- 4 decimal places
  } deriving (Eq, Ord, Show)

instance S.Binary IPOQuotingPeriodUpdate where
  get = IPOQuotingPeriodUpdate <$> S.getWord16be <*> S.getWord16be <*> S.get <*> (BS.toShort <$> S.getByteString 8)
                               <*> S.getWord32be <*> S.get <*> S.getWord32be
  put = undefined

instance MessageBasics IPOQuotingPeriodUpdate where
  stockLocate    = qpu_stockLocate
  trackingNumber = qpu_trackingNumber
  timestamp      = qpu_timestamp

--------------------------------------------------------------------------------

data AddOrder = AddOrder
  { ao_stockLocate                 :: {-# UNPACK #-} !Word16
  , ao_trackingNumber              :: {-# UNPACK #-} !Word16
  , ao_timestamp                   :: {-# UNPACK #-} !TimeStamp6
  , ao_orderReferenceNumber        :: {-# UNPACK #-} !Word64
  , ao_buySellIndicator            :: {-# UNPACK #-} !Char
  , ao_shares                      :: {-# UNPACK #-} !Word32
  , ao_stock                       :: {-# UNPACK #-} !ShortByteString -- 8
  , ao_price                       :: {-# UNPACK #-} !Word32 -- 4 decimal places
  } deriving (Eq, Ord, Show)

instance S.Binary AddOrder where
  get = AddOrder <$> S.getWord16be <*> S.getWord16be <*> S.get <*> S.getWord64be <*> S.get
                 <*> S.getWord32be <*> (BS.toShort <$> S.getByteString 8) <*> S.getWord32be
  put = undefined

instance MessageBasics AddOrder where
  stockLocate    = ao_stockLocate
  trackingNumber = ao_trackingNumber
  timestamp      = ao_timestamp

--------------------------------------------------------------------------------

data AddOrderMPIDAttr = AddOrderMPIDAttr
  { aoa_stockLocate                 :: {-# UNPACK #-} !Word16
  , aoa_trackingNumber              :: {-# UNPACK #-} !Word16
  , aoa_timestamp                   :: {-# UNPACK #-} !TimeStamp6
  , aoa_orderReferenceNumber        :: {-# UNPACK #-} !Word64
  , aoa_buySellIndicator            :: {-# UNPACK #-} !Char
  , aoa_shares                      :: {-# UNPACK #-} !Word32
  , aoa_stock                       :: {-# UNPACK #-} !ShortByteString -- 8
  , aoa_price                       :: {-# UNPACK #-} !Word32 -- 4 decimal places
  , aoa_attribution                 :: {-# UNPACK #-} !ShortByteString -- 4
  } deriving (Eq, Ord, Show)

instance S.Binary AddOrderMPIDAttr where
  get = AddOrderMPIDAttr <$> S.getWord16be <*> S.getWord16be <*> S.get <*> S.getWord64be <*> S.get
                         <*> S.getWord32be <*> (BS.toShort <$> S.getByteString 8) <*> S.getWord32be
                         <*> (BS.toShort <$> S.getByteString 4)
  put = undefined

instance MessageBasics AddOrderMPIDAttr where
  stockLocate    = aoa_stockLocate
  trackingNumber = aoa_trackingNumber
  timestamp      = aoa_timestamp

--------------------------------------------------------------------------------

data OrderExecuted = OrderExecuted
  { oe_stockLocate                 :: {-# UNPACK #-} !Word16
  , oe_trackingNumber              :: {-# UNPACK #-} !Word16
  , oe_timestamp                   :: {-# UNPACK #-} !TimeStamp6
  , oe_orderReferenceNumber        :: {-# UNPACK #-} !Word64
  , oe_executedShares              :: {-# UNPACK #-} !Word32
  , oe_matchNumber                 :: {-# UNPACK #-} !Word64
  } deriving (Eq, Ord, Show)

instance S.Binary OrderExecuted where
  get = OrderExecuted <$> S.getWord16be <*> S.getWord16be <*> S.get <*> S.getWord64be
                      <*> S.getWord32be <*> S.getWord64be
  put = undefined

instance MessageBasics OrderExecuted where
  stockLocate    = oe_stockLocate
  trackingNumber = oe_trackingNumber
  timestamp      = oe_timestamp

--------------------------------------------------------------------------------

data OrderExecutedWithPrice = OrderExecutedWithPrice
  { oewp_stockLocate                 :: {-# UNPACK #-} !Word16
  , oewp_trackingNumber              :: {-# UNPACK #-} !Word16
  , oewp_timestamp                   :: {-# UNPACK #-} !TimeStamp6
  , oewp_orderReferenceNumber        :: {-# UNPACK #-} !Word64
  , oewp_executedShares              :: {-# UNPACK #-} !Word32
  , oewp_matchNumber                 :: {-# UNPACK #-} !Word64
  , oewp_printable                   :: {-# UNPACK #-} !Char
  , oewp_executionPrice              :: {-# UNPACK #-} !Word32
  } deriving (Eq, Ord, Show)

instance S.Binary OrderExecutedWithPrice where
  get = OrderExecutedWithPrice <$> S.getWord16be <*> S.getWord16be <*> S.get <*> S.getWord64be
                               <*> S.getWord32be <*> S.getWord64be <*> S.get <*> S.getWord32be
  put = undefined

instance MessageBasics OrderExecutedWithPrice where
  stockLocate    = oewp_stockLocate
  trackingNumber = oewp_trackingNumber
  timestamp      = oewp_timestamp

--------------------------------------------------------------------------------

data OrderCancel = OrderCancel
  { oc_stockLocate                 :: {-# UNPACK #-} !Word16
  , oc_trackingNumber              :: {-# UNPACK #-} !Word16
  , oc_timestamp                   :: {-# UNPACK #-} !TimeStamp6
  , oc_orderReferenceNumber        :: {-# UNPACK #-} !Word64
  , oc_cancelledShares             :: {-# UNPACK #-} !Word32
  } deriving (Eq, Ord, Show)

instance S.Binary OrderCancel where
  get = OrderCancel <$> S.getWord16be <*> S.getWord16be <*> S.get <*> S.getWord64be <*> S.getWord32be
  put = undefined

instance MessageBasics OrderCancel where
  stockLocate    = oc_stockLocate
  trackingNumber = oc_trackingNumber
  timestamp      = oc_timestamp

--------------------------------------------------------------------------------

data OrderDelete = OrderDelete
  { od_stockLocate                 :: {-# UNPACK #-} !Word16
  , od_trackingNumber              :: {-# UNPACK #-} !Word16
  , od_timestamp                   :: {-# UNPACK #-} !TimeStamp6
  , od_orderReferenceNumber        :: {-# UNPACK #-} !Word64
  } deriving (Eq, Ord, Show)

instance S.Binary OrderDelete where
  get = OrderDelete <$> S.getWord16be <*> S.getWord16be <*> S.get <*> S.getWord64be
  put = undefined

instance MessageBasics OrderDelete where
  stockLocate    = od_stockLocate
  trackingNumber = od_trackingNumber
  timestamp      = od_timestamp

--------------------------------------------------------------------------------

data OrderReplace = OrderReplace
  { or_stockLocate                  :: {-# UNPACK #-} !Word16
  , or_trackingNumber               :: {-# UNPACK #-} !Word16
  , or_timestamp                    :: {-# UNPACK #-} !TimeStamp6
  , or_originalOrderReferenceNumber :: {-# UNPACK #-} !Word64
  , or_newOrderReferenceNumber      :: {-# UNPACK #-} !Word64
  , or_shares                       :: {-# UNPACK #-} !Word32
  , or_price                        :: {-# UNPACK #-} !Word32 -- 4 decimal places
  } deriving (Eq, Ord, Show)

instance S.Binary OrderReplace where
  get = OrderReplace <$> S.getWord16be <*> S.getWord16be <*> S.get <*> S.getWord64be <*> S.getWord64be
                     <*> S.getWord32be <*> S.getWord32be
  put = undefined

instance MessageBasics OrderReplace where
  stockLocate    = or_stockLocate
  trackingNumber = or_trackingNumber
  timestamp      = or_timestamp

--------------------------------------------------------------------------------

data Trade = Trade
  { t_stockLocate                  :: {-# UNPACK #-} !Word16
  , t_trackingNumber               :: {-# UNPACK #-} !Word16
  , t_timestamp                    :: {-# UNPACK #-} !TimeStamp6
  , t_orderReferenceNumber         :: {-# UNPACK #-} !Word64
  , t_buySellIndicator             :: {-# UNPACK #-} !Char
  , t_shares                       :: {-# UNPACK #-} !Word32
  , t_stock                        :: {-# UNPACK #-} !ShortByteString -- 8
  , t_price                        :: {-# UNPACK #-} !Word32 -- 4 decimal places
  , t_matchNumer                   :: {-# UNPACK #-} !Word64
  } deriving (Eq, Ord, Show)

instance S.Binary Trade where
  get = Trade <$> S.getWord16be <*> S.getWord16be <*> S.get <*> S.getWord64be <*> S.get
              <*> S.getWord32be <*> (BS.toShort <$> S.getByteString 8) <*> S.getWord32be
              <*> S.getWord64be
  put = undefined

instance MessageBasics Trade where
  stockLocate    = t_stockLocate
  trackingNumber = t_trackingNumber
  timestamp      = t_timestamp

--------------------------------------------------------------------------------

data CrossTrade = CrossTrade
  { ct_stockLocate                  :: {-# UNPACK #-} !Word16
  , ct_trackingNumber               :: {-# UNPACK #-} !Word16
  , ct_timestamp                    :: {-# UNPACK #-} !TimeStamp6
  , ct_shares                       :: {-# UNPACK #-} !Word64
  , ct_stock                        :: {-# UNPACK #-} !ShortByteString -- 8
  , ct_price                        :: {-# UNPACK #-} !Word32 -- 4 decimal places
  , ct_matchNumber                  :: {-# UNPACK #-} !Word64
  , ct_crossType                    :: {-# UNPACK #-} !Char
  } deriving (Eq, Ord, Show)

instance S.Binary CrossTrade where
  get = CrossTrade <$> S.getWord16be <*> S.getWord16be <*> S.get <*> S.getWord64be
                   <*> (BS.toShort <$> S.getByteString 8) <*> S.getWord32be
                   <*> S.getWord64be <*> S.get
  put = undefined

instance MessageBasics CrossTrade where
  stockLocate    = ct_stockLocate
  trackingNumber = ct_trackingNumber
  timestamp      = ct_timestamp

--------------------------------------------------------------------------------

data BrokenTrade = BrokenTrade
  { bt_stockLocate                  :: {-# UNPACK #-} !Word16
  , bt_trackingNumber               :: {-# UNPACK #-} !Word16
  , bt_timestamp                    :: {-# UNPACK #-} !TimeStamp6
  , bt_matchNumber                  :: {-# UNPACK #-} !Word64
  } deriving (Eq, Ord, Show)

instance S.Binary BrokenTrade where
  get = BrokenTrade <$> S.getWord16be <*> S.getWord16be <*> S.get <*> S.getWord64be
  put = undefined

instance MessageBasics BrokenTrade where
  stockLocate    = bt_stockLocate
  trackingNumber = bt_trackingNumber
  timestamp      = bt_timestamp

--------------------------------------------------------------------------------

data NOII = NOII
  { noii_stockLocate                  :: {-# UNPACK #-} !Word16
  , noii_trackingNumber               :: {-# UNPACK #-} !Word16
  , noii_timestamp                    :: {-# UNPACK #-} !TimeStamp6
  , noii_paidShares                   :: {-# UNPACK #-} !Word64
  , noii_imbalanceShares              :: {-# UNPACK #-} !Word64
  , noii_imbalanceDirection           :: {-# UNPACK #-} !Char
  , noii_stock                        :: {-# UNPACK #-} !ShortByteString
  , noii_farPrice                     :: {-# UNPACK #-} !Word32
  , noii_nearPrice                    :: {-# UNPACK #-} !Word32
  , noii_currentReferencePrice        :: {-# UNPACK #-} !Word32
  , noii_crossType                    :: {-# UNPACK #-} !Char
  , noii_priceVariationIndicator      :: {-# UNPACK #-} !Char
  } deriving (Eq, Ord, Show)

instance S.Binary NOII where
  get = NOII <$> S.getWord16be <*> S.getWord16be <*> S.get
             <*> S.getWord64be <*> S.getWord64be <*> S.get
             <*> (BS.toShort <$> S.getByteString 8)
             <*> S.getWord32be <*> S.getWord32be <*> S.getWord32be
             <*> S.get <*> S.get
  put = undefined

instance MessageBasics NOII where
  stockLocate    = noii_stockLocate
  trackingNumber = noii_trackingNumber
  timestamp      = noii_timestamp

--------------------------------------------------------------------------------

data RPII = RPII
  { rpii_stockLocate                  :: {-# UNPACK #-} !Word16
  , rpii_trackingNumber               :: {-# UNPACK #-} !Word16
  , rpii_timestamp                    :: {-# UNPACK #-} !TimeStamp6
  , rpii_stock                        :: {-# UNPACK #-} !ShortByteString
  , rpii_interestFlag                 :: {-# UNPACK #-} !Char
  } deriving (Eq, Ord, Show)

instance S.Binary RPII where
  get = RPII <$> S.getWord16be <*> S.getWord16be <*> S.get
             <*> (BS.toShort <$> S.getByteString 8)
             <*> S.get
  put = undefined

instance MessageBasics RPII where
  stockLocate    = rpii_stockLocate
  trackingNumber = rpii_trackingNumber
  timestamp      = rpii_timestamp

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
             | MAddOrder AddOrder
             | MAddOrderMPIDAttr AddOrderMPIDAttr
             | MOrderExecuted OrderExecuted
             | MOrderExecutedWithPrice OrderExecutedWithPrice
             | MOrderCancel OrderCancel
             | MOrderDelete OrderDelete
             | MOrderReplace OrderReplace
             | MTrade Trade
             | MCrossTrade CrossTrade
             | MBrokenTrade BrokenTrade
             | MNOII NOII
             | MRPII RPII
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
      'A' -> MAddOrder                  <$> S.get
      'F' -> MAddOrderMPIDAttr          <$> S.get
      'E' -> MOrderExecuted             <$> S.get
      'C' -> MOrderExecutedWithPrice    <$> S.get
      'X' -> MOrderCancel               <$> S.get
      'D' -> MOrderDelete               <$> S.get
      'U' -> MOrderReplace              <$> S.get
      'P' -> MTrade                     <$> S.get
      'Q' -> MCrossTrade                <$> S.get
      'B' -> MBrokenTrade               <$> S.get
      'I' -> MNOII                      <$> S.get
      'N' -> MRPII                      <$> S.get
      _   -> MOther c                   <$> S.get
  put = undefined

instance MessageBasics Message where
  stockLocate (MSystemEvent               x) = stockLocate x
  stockLocate (MStockDirectory            x) = stockLocate x
  stockLocate (MStockTradingAction        x) = stockLocate x
  stockLocate (MREGSHORestriction         x) = stockLocate x
  stockLocate (MMarketParticipantPosition x) = stockLocate x
  stockLocate (MMWCBDeclineLevel          x) = stockLocate x
  stockLocate (MMWCBBreach                x) = stockLocate x
  stockLocate (MIPOQuotingPeriodUpdate    x) = stockLocate x
  stockLocate (MAddOrder                  x) = stockLocate x
  stockLocate (MAddOrderMPIDAttr          x) = stockLocate x
  stockLocate (MOrderExecuted             x) = stockLocate x
  stockLocate (MOrderExecutedWithPrice    x) = stockLocate x
  stockLocate (MOrderCancel               x) = stockLocate x
  stockLocate (MOrderDelete               x) = stockLocate x
  stockLocate (MOrderReplace              x) = stockLocate x
  stockLocate (MTrade                     x) = stockLocate x
  stockLocate (MCrossTrade                x) = stockLocate x
  stockLocate (MBrokenTrade               x) = stockLocate x
  stockLocate (MNOII                      x) = stockLocate x
  stockLocate (MRPII                      x) = stockLocate x
  trackingNumber = undefined
  timestamp      = undefined

messageType :: Message -> Char               
messageType (MSystemEvent               _) = 'S'
messageType (MStockDirectory            _) = 'R'
messageType (MStockTradingAction        _) = 'H'
messageType (MREGSHORestriction         _) = 'Y'
messageType (MMarketParticipantPosition _) = 'L'
messageType (MMWCBDeclineLevel          _) = 'V'
messageType (MMWCBBreach                _) = 'W'
messageType (MIPOQuotingPeriodUpdate    _) = 'K'
messageType (MAddOrder                  _) = 'A'
messageType (MAddOrderMPIDAttr          _) = 'F'
messageType (MOrderExecuted             _) = 'E'
messageType (MOrderExecutedWithPrice    _) = 'C'
messageType (MOrderCancel               _) = 'X'
messageType (MOrderDelete               _) = 'D'
messageType (MOrderReplace              _) = 'U'
messageType (MTrade                     _) = 'P'
messageType (MCrossTrade                _) = 'Q'
messageType (MBrokenTrade               _) = 'B'
messageType (MNOII                      _) = 'I'
messageType (MRPII                      _) = 'N'
