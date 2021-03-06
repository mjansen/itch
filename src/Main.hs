{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad
import Control.DeepSeq

import Data.Word

import qualified Data.Binary            as S
import qualified Data.Binary.Get        as S
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Lazy   as L
import qualified Data.ByteString.Short  as BS

import qualified Data.Set               as Set
import qualified Data.IntMap.Strict        as Map

import qualified Codec.Compression.GZip as GZ

import qualified Options.Applicative as OA

import System.Environment

import GHC.Generics

import qualified Data.ITCH.NASDAQ.NASDAQ50.Messages as N50

type ShortByteString = BS.ShortByteString

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

showRecords :: FilePath -> IO ()
showRecords fileName =
  mapM_ printMessage =<< (decodeToList . GZ.decompress <$> L.readFile fileName)
  
printMessage :: Message -> IO ()
printMessage msg@(Message len con) = do
  -- print msg
  case S.decode con :: N50.Message of
    N50.MOther _ _ -> return ()
    m              -> print m

decodeMessage :: Message -> N50.Message
decodeMessage msg@(Message len con) = S.decode con

showOrders1 :: FilePath -> IO ()
showOrders1 fileName =
  print . onlyIncreases . map Set.size =<< (consolidate Set.empty . decodeToList . GZ.decompress <$> L.readFile fileName)

showOrders2 :: Word16 -> FilePath -> IO ()
showOrders2 n fileName =
  mapM_ print =<< (filter ((== n) . N50.stockLocate) . map decodeMessage . decodeToList . GZ.decompress <$> L.readFile fileName)

showOrders3 :: Word16 -> FilePath -> IO ()
showOrders3 n fileName =
  mapM_ print =<< (consolidate2 (OrderBook BS.empty Set.empty Set.empty) . filter ((== n) . N50.stockLocate) . map decodeMessage . decodeToList . GZ.decompress <$> L.readFile fileName)

showOrders4 :: FilePath -> IO ()
showOrders4 fileName =
  mapM_ print . Map.toList =<< (consolidate3 Map.empty . map decodeMessage . decodeToList . GZ.decompress <$> L.readFile fileName)

showOrders5 :: FilePath -> IO ()
showOrders5 fileName =
  mapM_ print . Map.toList =<< (consolidate4 Map.empty . map decodeMessage . decodeToList . GZ.decompress <$> L.readFile fileName)

consolidate :: Set.Set Word64 -> [Message] -> [Set.Set Word64]
consolidate s [] = []
consolidate s (m:ms) =
  let s' = processOrder (S.decode . m_content $ m) s
  in s' : consolidate s' ms

onlyIncreases :: [Int] -> [Int]
onlyIncreases (x:y:zs) | x >= y = onlyIncreases (x:zs)
                       | x <  y = x:onlyIncreases (y:zs)
onlyIncreases zs = zs

processOrder :: N50.Message -> Set.Set Word64 -> Set.Set Word64
processOrder (N50.MAddOrder         x) s = Set.insert ( N50.ao_orderReferenceNumber x) s
processOrder (N50.MAddOrderMPIDAttr x) s = Set.insert (N50.aoa_orderReferenceNumber x) s
processOrder (N50.MOrderExecuted    x) s = s
processOrder (N50.MOrderExecutedWithPrice x) s = s
processOrder (N50.MOrderCancel      x) s = Set.delete (N50.oc_orderReferenceNumber x) s
processOrder (N50.MOrderDelete      x) s = Set.delete (N50.od_orderReferenceNumber x) s
processOrder (N50.MOrderReplace     x) s = N50.or_newOrderReferenceNumber x `Set.insert` Set.delete (N50.or_originalOrderReferenceNumber x) s
processOrder (N50.MTrade            x) s = s
processOrder (N50.MCrossTrade       x) s = s
processOrder (N50.MBrokenTrade      x) s = s
processOrder _                         s = s

--------------------------------------------------------------------------------

data OrderBook = OrderBook
  { ob_stock :: {-# UNPACK #-} !ShortByteString
  , ob_bids  :: {-# UNPACK #-} !(Set.Set BidAsk)
  , ob_asks  :: {-# UNPACK #-} !(Set.Set BidAsk)
  } deriving (Eq, Ord, Show, Generic)

data OrderBook2 = OrderBook2
  { ob2_stock :: {-# UNPACK #-} !ShortByteString
  , ob2_bids  :: {-# UNPACK #-} !(Map.IntMap BidAsk2)
  , ob2_asks  :: {-# UNPACK #-} !(Map.IntMap BidAsk2)
  } deriving (Eq, Ord, Show, Generic)

instance NFData OrderBook

instance NFData OrderBook2

data BidAsk = BidAsk
  { ba_referenceNumber :: {-# UNPACK #-} !Word64
  , ba_shares          :: {-# UNPACK #-} !Word32
  , ba_price           :: {-# UNPACK #-} !Word32
  , ba_timestamp       :: {-# UNPACK #-} !N50.TimeStamp6
  } deriving (Eq, Ord, Show, Generic)

data BidAsk2 = BidAsk2
  { ba2_shares         :: {-# UNPACK #-} !Word32
  , ba2_price          :: {-# UNPACK #-} !Word32
  , ba2_timestamp      :: {-# UNPACK #-} !N50.TimeStamp6
  } deriving (Eq, Ord, Show, Generic)

instance NFData BidAsk

instance NFData BidAsk2

data Order = OBid BidAsk | OAsk BidAsk
           deriving (Eq, Ord, Show)

toBidAsk :: Order -> BidAsk
toBidAsk (OBid x) = x
toBidAsk (OAsk x) = x

toBidAsk2 :: Order -> BidAsk2
toBidAsk2 (OBid x) = BidAsk2 (ba_shares x) (ba_price x) (ba_timestamp x)
toBidAsk2 (OAsk x) = BidAsk2 (ba_shares x) (ba_price x) (ba_timestamp x)

toBidAsk2' :: BidAsk -> BidAsk2
toBidAsk2' x = BidAsk2 (ba_shares x) (ba_price x) (ba_timestamp x)

addToOrderBook = addToOrderBookNZ

addToOrderBookA :: Order -> OrderBook -> OrderBook
addToOrderBookA (OBid o) ob = ob { ob_bids = o `Set.insert` ob_bids ob }
addToOrderBookA (OAsk o) ob = ob { ob_asks = o `Set.insert` ob_asks ob }

addToOrderBookNZ :: Order -> OrderBook -> OrderBook
addToOrderBookNZ (OBid o) ob | ba_shares o == 0 = ob
                             | ba_shares o >  0 = ob { ob_bids = o `Set.insert` ob_bids ob }
                             | otherwise        = error $ "attempt to add bid order for a negative nr of shares: " ++ show o
addToOrderBookNZ (OAsk o) ob | ba_shares o == 0 = ob
                             | ba_shares o >  0 = ob { ob_asks = o `Set.insert` ob_asks ob }
                             | otherwise        = error $ "attempt to add ask order for a negative nr of shares: " ++ show o

addToOrderBook2' :: Order -> OrderBook2 -> OrderBook2
addToOrderBook2' (OBid o) ob | ba_shares o == 0 = ob
                             | ba_shares o >  0 = ob { ob2_bids = Map.insert (fromIntegral $ ba_referenceNumber o) (toBidAsk2' o) (ob2_bids ob) }
                             | otherwise        = error $ "attempt to add bid order for a negative nr of shares: " ++ show o
addToOrderBook2' (OAsk o) ob | ba_shares o == 0 = ob
                             | ba_shares o >  0 = ob { ob2_asks = Map.insert (fromIntegral $ ba_referenceNumber o) (toBidAsk2' o) (ob2_asks ob) }
                             | otherwise        = error $ "attempt to add ask order for a negative nr of shares: " ++ show o

addToOrderBook2 :: Order -> OrderBook2 -> OrderBook2
addToOrderBook2 o@(OBid (BidAsk rn sh pr ts)) ob =
  let ob' = addToOrderBook2' o ob
      v = lookupOrderInBook2 rn ob'
  in seq v ob'
addToOrderBook2 o@(OAsk (BidAsk rn sh pr ts)) ob =
  let ob' = addToOrderBook2' o ob
      v = lookupOrderInBook2 rn ob'
  in seq v ob'

deleteFromOrderBook :: Order -> OrderBook -> OrderBook
deleteFromOrderBook (OBid o) ob = ob { ob_bids = o `Set.delete` ob_bids ob }
deleteFromOrderBook (OAsk o) ob = ob { ob_asks = o `Set.delete` ob_asks ob }

deleteFromOrderBook2 :: Word64 -> OrderBook2 -> OrderBook2
deleteFromOrderBook2 refNum ob =
  case lookupOrderInBook2 refNum ob of
    Nothing        -> error $ "trying to delete order that does not exist: " ++ show refNum
    Just (OBid _)  -> ob { ob2_bids = Map.delete (fromIntegral refNum) (ob2_bids ob) }
    Just (OAsk _)  -> ob { ob2_asks = Map.delete (fromIntegral refNum) (ob2_asks ob) }

lookupOrderInBook :: Word64 -> OrderBook -> Maybe Order
lookupOrderInBook refNum ob =
  let o = BidAsk refNum 0 0 (N50.T6 0)
      lookMoreClosely o'' = if refNum == ba_referenceNumber o'' then Just o'' else Nothing
  in case ( lookMoreClosely =<< Set.lookupGE o (ob_asks ob)
          , lookMoreClosely =<< Set.lookupGE o (ob_bids ob) ) of
       ( Just o', _       ) -> Just (OAsk o')
       ( Nothing, Just o' ) -> Just (OBid o')
       ( Nothing, Nothing ) -> Nothing

lookupOrderInBook2 :: Word64 -> OrderBook2 -> Maybe Order
lookupOrderInBook2 refNum ob =
  case ( Map.lookup (fromIntegral refNum) (ob2_bids ob)
       , Map.lookup (fromIntegral refNum) (ob2_asks ob) ) of
    ( Just (BidAsk2 sh pr ts), _ )       -> Just . OBid $ BidAsk refNum sh pr ts
    ( Nothing, Just (BidAsk2 sh pr ts) ) -> Just . OAsk $ BidAsk refNum sh pr ts
    ( Nothing, Nothing )                 -> Nothing

subtractFromOrderBook2 :: Word64 -> Word32 -> N50.TimeStamp6 -> OrderBook2 -> OrderBook2
subtractFromOrderBook2 refNum shares ts ob =
  case lookupOrderInBook2 refNum ob of
    Nothing -> error $ "subtractFromOrderBook:could not find order " ++ show refNum
    Just (OBid o)  -> addToOrderBook2 (OBid $ o { ba_shares = ba_shares o - shares, ba_timestamp = ts }) . deleteFromOrderBook2 refNum $ ob
    Just (OAsk o)  -> addToOrderBook2 (OAsk $ o { ba_shares = ba_shares o - shares, ba_timestamp = ts }) . deleteFromOrderBook2 refNum $ ob

subtractFromOrderBook :: Word64 -> Word32 -> N50.TimeStamp6 -> OrderBook -> OrderBook
subtractFromOrderBook refNum shares ts ob =
  case lookupOrderInBook refNum ob of
    Nothing -> error $ "subtractFromOrderBook:could not find order " ++ show refNum
    Just (OBid o)  -> addToOrderBook (OBid $ o { ba_shares = ba_shares o - shares, ba_timestamp = ts }) . deleteFromOrderBook (OBid o) $ ob
    Just (OAsk o)  -> addToOrderBook (OAsk $ o { ba_shares = ba_shares o - shares, ba_timestamp = ts }) . deleteFromOrderBook (OAsk o) $ ob

fromAddOrder :: N50.AddOrder -> Order
fromAddOrder (N50.AddOrder sl tn ts rn bs sh st pr)
  | bs == 'B' = OBid $ BidAsk rn sh pr ts
  | bs == 'S' = OAsk $ BidAsk rn sh pr ts
  | otherwise = error "neither bid nor ask"

fromAddOrderMPIDAttr :: N50.AddOrderMPIDAttr -> Order
fromAddOrderMPIDAttr (N50.AddOrderMPIDAttr sl tn ts rn bs sh st pr at)
  | bs == 'B' = OBid $ BidAsk rn sh pr ts
  | bs == 'S' = OAsk $ BidAsk rn sh pr ts
  | otherwise = error "neither bid nor ask"

-- type OrderBooks = Map.Map ShortByteString OrderBook

pOrder :: N50.Message -> OrderBook -> OrderBook
pOrder (N50.MAddOrder         x) ob = addToOrderBook (fromAddOrder         x) ob
pOrder (N50.MAddOrderMPIDAttr x) ob = addToOrderBook (fromAddOrderMPIDAttr x) ob
pOrder (N50.MOrderExecuted    x) ob =
  let refNum = N50.oe_orderReferenceNumber x
  in subtractFromOrderBook refNum (N50.oe_executedShares x) (N50.oe_timestamp x) ob
pOrder (N50.MOrderExecutedWithPrice x) ob =
  let refNum = N50.oewp_orderReferenceNumber x
  in subtractFromOrderBook refNum (N50.oewp_executedShares x) (N50.oewp_timestamp x) ob
pOrder (N50.MOrderCancel      x) ob =
  let refNum = N50.oc_orderReferenceNumber x
  in subtractFromOrderBook refNum (N50.oc_cancelledShares x) (N50.oc_timestamp x) ob
pOrder (N50.MOrderDelete      x) ob =
  let refNum = N50.od_orderReferenceNumber x
  in case lookupOrderInBook refNum ob of
       Nothing -> error $ "delete: cannot find order " ++ show refNum
       Just o  -> deleteFromOrderBook o ob
pOrder (N50.MOrderReplace     x) ob =
  let refNum1 = N50.or_originalOrderReferenceNumber x
  in case lookupOrderInBook refNum1 ob of
       Nothing -> error $ "replace: cannot find order " ++ show refNum1
       Just o  -> let ba  = (toBidAsk o)
                      ba' = ba { ba_referenceNumber = N50.or_newOrderReferenceNumber x
                               , ba_shares          = N50.or_shares x
                               , ba_price           = N50.or_price x
                               , ba_timestamp       = N50.or_timestamp x
                               }
                      o'  = case o of OBid _ -> OBid ba'; OAsk _ -> OAsk ba'
                  in addToOrderBook o' . deleteFromOrderBook o $ ob
pOrder (N50.MTrade            x) ob = ob
pOrder (N50.MCrossTrade       x) ob = ob
pOrder (N50.MBrokenTrade      x) ob = ob
pOrder _                         ob = ob

pOrder2 :: N50.Message -> OrderBook2 -> OrderBook2
pOrder2 (N50.MAddOrder         x) ob = addToOrderBook2 (fromAddOrder         x) ob
pOrder2 (N50.MAddOrderMPIDAttr x) ob = addToOrderBook2 (fromAddOrderMPIDAttr x) ob
pOrder2 (N50.MOrderExecuted    x) ob =
  let refNum = N50.oe_orderReferenceNumber x
  in subtractFromOrderBook2 refNum (N50.oe_executedShares x) (N50.oe_timestamp x) ob
pOrder2 (N50.MOrderExecutedWithPrice x) ob =
  let refNum = N50.oewp_orderReferenceNumber x
  in subtractFromOrderBook2 refNum (N50.oewp_executedShares x) (N50.oewp_timestamp x) ob
pOrder2 (N50.MOrderCancel      x) ob =
  let refNum = N50.oc_orderReferenceNumber x
  in subtractFromOrderBook2 refNum (N50.oc_cancelledShares x) (N50.oc_timestamp x) ob
pOrder2 (N50.MOrderDelete      x) ob =
  let refNum = N50.od_orderReferenceNumber x
  in case lookupOrderInBook2 refNum ob of
       Nothing -> error $ "delete: cannot find order " ++ show refNum
       Just o  -> deleteFromOrderBook2 refNum ob
pOrder2 (N50.MOrderReplace     x) ob =
  let refNum1 = N50.or_originalOrderReferenceNumber x
  in case lookupOrderInBook2 refNum1 ob of
       Nothing -> error $ "replace: cannot find order " ++ show refNum1
       Just o  -> let ba  = (toBidAsk o)
                      ba' = ba { ba_referenceNumber = N50.or_newOrderReferenceNumber x
                               , ba_shares          = N50.or_shares x
                               , ba_price           = N50.or_price x
                               , ba_timestamp       = N50.or_timestamp x
                               }
                      o'  = case o of OBid _ -> OBid ba'; OAsk _ -> OAsk ba'
                  in addToOrderBook2 o' . deleteFromOrderBook2 refNum1 $ ob
pOrder2 (N50.MTrade            x) ob = ob
pOrder2 (N50.MCrossTrade       x) ob = ob
pOrder2 (N50.MBrokenTrade      x) ob = ob
pOrder2 _                         ob = ob

consolidate2 :: OrderBook -> [N50.Message] -> [OrderBook]
consolidate2 s [] = []
consolidate2 s (m:ms) =
  let s' = pOrder m s
  in s' : consolidate2 s' ms

--------------------------------------------------------------------------------

type OrderBooks = Map.IntMap {- Word16 -} OrderBook

type OrderBooks2 = Map.IntMap OrderBook2

-- (OrderBook BS.empty Set.empty Set.empty)

consolidate3 :: OrderBooks -> [N50.Message] -> OrderBooks
consolidate3 bs [] = bs
consolidate3 bs (m:ms) =
  let n   = fromIntegral $ N50.stockLocate m
      b   = maybe (OrderBook BS.empty Set.empty Set.empty) id . Map.lookup n $ bs
      b'  = pOrder m b
      bs' = deepseq b $ Map.insert n b' bs
  in seq bs' $ consolidate3 bs' ms

consolidate4 :: OrderBooks2 -> [N50.Message] -> OrderBooks2
consolidate4 bs [] = bs
consolidate4 bs (m:ms) =
  let n   = fromIntegral $ N50.stockLocate m
      b   = maybe (OrderBook2 BS.empty Map.empty Map.empty) id . Map.lookup n $ bs
      b'  = pOrder2 m b
      bs' = Map.insert n b' bs
      b'' = Map.lookup n bs'
  in seq b'' $ consolidate4 bs' ms

--------------------------------------------------------------------------------

opts :: OA.Parser (IO ())
opts = OA.subparser
  (     OA.command "show"    (OA.info (showRecords <$> OA.argument OA.str OA.idm) (OA.progDesc "show all records in TARGET"))
  OA.<> OA.command "orders1" (OA.info (showOrders1 <$> OA.argument OA.str OA.idm) (OA.progDesc "show orders1 in TARGET"))
  OA.<> OA.command "orders2" (OA.info (showOrders2 <$> (read <$> OA.argument OA.str OA.idm) <*> OA.argument OA.str OA.idm) (OA.progDesc "show orders2 in TARGET"))
  OA.<> OA.command "orders3" (OA.info (showOrders3 <$> (read <$> OA.argument OA.str OA.idm) <*> OA.argument OA.str OA.idm) (OA.progDesc "show orders3 in TARGET"))
  OA.<> OA.command "orders4" (OA.info (showOrders4 <$> OA.argument OA.str OA.idm) (OA.progDesc "show all outstanding order book entries at the end of trading in TARGET"))
  OA.<> OA.command "orders42" (OA.info (showOrders5 <$> OA.argument OA.str OA.idm) (OA.progDesc "show all outstanding order book entries at the end of trading in TARGET"))
  OA.<> OA.command "stop"    (OA.info (pure (return ())) OA.idm) )

main :: IO ()
main = join $ OA.execParser (OA.info opts OA.idm)
