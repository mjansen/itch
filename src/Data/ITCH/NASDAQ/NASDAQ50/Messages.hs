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

class MessageBasics a where
  stockLocate    :: a -> Word16
  trackingNumber :: a -> Word16
  timestamp      :: a -> TimeStamp6

data SystemEvent = SystemEvent
  { sem_stockLocate    :: Word16
  , sem_trackingNumber :: Word16
  , sem_timestamp      :: TimeStamp6
  , sem_eventCode      :: Char
  } deriving (Eq, Ord, Show)

instance S.Binary SystemEvent where
  get = SystemEvent <$> S.getWord16be <*> S.getWord16be <*> S.get <*> S.get
  put (SystemEvent a b c d) = S.put a >> S.put b >> S.put c >> S.put d

instance MessageBasics SystemEvent where
  stockLocate    = sem_stockLocate
  trackingNumber = sem_trackingNumber
  timestamp      = sem_timestamp
  
data StockDirectory = StockDirectory
  { sdm_stockLocate                 :: Word16
  , sdm_trackingNumber              :: Word16
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

instance MessageBasics StockDirectory where
  stockLocate    = sdm_stockLocate
  trackingNumber = sdm_trackingNumber
  timestamp      = sdm_timestamp
  
data StockTradingAction = StockTradingAction
  { sta_stockLocate                 :: Word16
  , sta_trackingNumber              :: Word16
  , sta_timestamp                   :: TimeStamp6
  , sta_stock                       :: ShortByteString -- 8
  , sta_tradingState                :: Char
  , sta_reserved                    :: Char
  , sta_reason                      :: ShortByteString -- 4
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
  { rsr_stockLocate                 :: Word16
  , rsr_trackingNumber              :: Word16
  , rsr_timestamp                   :: TimeStamp6
  , rsr_stock                       :: ShortByteString -- 8
  , rsr_REGSHOAction                :: Char
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
  { mpp_stockLocate                 :: Word16
  , mpp_trackingNumber              :: Word16
  , mpp_timestamp                   :: TimeStamp6
  , mpp_mpid                        :: Word32
  , mpp_stock                       :: ShortByteString -- 8
  , mpp_primaryMarketMaker          :: Char
  , mpp_marketMakerMode             :: Char
  , mpp_marketPartcipantState       :: Char
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

instance MessageBasics MWCBDeclineLevel where
  stockLocate    = mdl_stockLocate
  trackingNumber = mdl_trackingNumber
  timestamp      = mdl_timestamp

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

instance MessageBasics MWCBBreach where
  stockLocate    = mbr_stockLocate
  trackingNumber = mbr_trackingNumber
  timestamp      = mbr_timestamp

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

instance MessageBasics IPOQuotingPeriodUpdate where
  stockLocate    = qpu_stockLocate
  trackingNumber = qpu_trackingNumber
  timestamp      = qpu_timestamp

--------------------------------------------------------------------------------

data AddOrder = AddOrder
  { ao_stockLocate                 :: Word16
  , ao_trackingNumber              :: Word16
  , ao_timestamp                   :: TimeStamp6
  , ao_orderReferenceNumber        :: Word64
  , ao_buySellIndicator            :: Char
  , ao_shares                      :: Word32
  , ao_stock                       :: ShortByteString -- 8
  , ao_price                       :: Word32 -- 4 decimal places
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
  { aoa_stockLocate                 :: Word16
  , aoa_trackingNumber              :: Word16
  , aoa_timestamp                   :: TimeStamp6
  , aoa_orderReferenceNumber        :: Word64
  , aoa_buySellIndicator            :: Char
  , aoa_shares                      :: Word32
  , aoa_stock                       :: ShortByteString -- 8
  , aoa_price                       :: Word32 -- 4 decimal places
  , aoa_attribution                 :: ShortByteString -- 4
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
  { oe_stockLocate                 :: Word16
  , oe_trackingNumber              :: Word16
  , oe_timestamp                   :: TimeStamp6
  , oe_orderReferenceNumber        :: Word64
  , oe_executedShares              :: Word32
  , oe_matchNumber                 :: Word64
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
  { oewp_stockLocate                 :: Word16
  , oewp_trackingNumber              :: Word16
  , oewp_timestamp                   :: TimeStamp6
  , oewp_orderReferenceNumber        :: Word64
  , oewp_executedShares              :: Word32
  , oewp_matchNumber                 :: Word64
  , oewp_printable                   :: Char
  , oewp_executionPrice              :: Word32
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
  { oc_stockLocate                 :: Word16
  , oc_trackingNumber              :: Word16
  , oc_timestamp                   :: TimeStamp6
  , oc_orderReferenceNumber        :: Word64
  , oc_cancelledShares             :: Word32
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
  { od_stockLocate                 :: Word16
  , od_trackingNumber              :: Word16
  , od_timestamp                   :: TimeStamp6
  , od_orderReferenceNumber        :: Word64
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
  { or_stockLocate                  :: Word16
  , or_trackingNumber               :: Word16
  , or_timestamp                    :: TimeStamp6
  , or_originalOrderReferenceNumber :: Word64
  , or_newOrderReferenceNumber      :: Word64
  , or_shares                       :: Word32
  , or_price                        :: Word32 -- 4 decimal places
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
  { t_stockLocate                  :: Word16
  , t_trackingNumber               :: Word16
  , t_timestamp                    :: TimeStamp6
  , t_orderReferenceNumber         :: Word64
  , t_buySellIndicator             :: Char
  , t_shares                       :: Word32
  , t_stock                        :: ShortByteString -- 8
  , t_price                        :: Word32 -- 4 decimal places
  , t_matchNumer                   :: Word64
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
  { ct_stockLocate                  :: Word16
  , ct_trackingNumber               :: Word16
  , ct_timestamp                    :: TimeStamp6
  , ct_shares                       :: Word64
  , ct_stock                        :: ShortByteString -- 8
  , ct_price                        :: Word32 -- 4 decimal places
  , ct_matchNumber                  :: Word64
  , ct_crossType                    :: Char
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
  { bt_stockLocate                  :: Word16
  , bt_trackingNumber               :: Word16
  , bt_timestamp                    :: TimeStamp6
  , bt_matchNumber                  :: Word64
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
  { noii_stockLocate                  :: Word16
  , noii_trackingNumber               :: Word16
  , noii_timestamp                    :: TimeStamp6
  , noii_paidShares                   :: Word64
  , noii_imbalanceShares              :: Word64
  , noii_imbalanceDirection           :: Char
  , noii_stock                        :: ShortByteString
  , noii_farPrice                     :: Word32
  , noii_nearPrice                    :: Word32
  , noii_currentReferencePrice        :: Word32
  , noii_crossType                    :: Char
  , noii_priceVariationIndicator      :: Char
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
  { rpii_stockLocate                  :: Word16
  , rpii_trackingNumber               :: Word16
  , rpii_timestamp                    :: TimeStamp6
  , rpii_stock                        :: ShortByteString
  , rpii_interestFlag                 :: Char
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
