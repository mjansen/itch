module Main where

import Control.Monad

import Data.Word

import qualified Data.Binary            as S
import qualified Data.Binary.Get        as S
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Lazy   as L
import qualified Data.ByteString.Short  as BS

import qualified Data.Set               as Set

import qualified Codec.Compression.GZip as GZ

import qualified Options.Applicative as OA

import System.Environment

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
  { ob_stock :: ShortByteString
  , ob_bids  :: Set.Set BidAsk
  , ob_asks  :: Set.Set BidAsk
  } deriving (Eq, Ord, Show)

data BidAsk = BidAsk
  { ba_referenceNumber :: Word64
  , ba_shares          :: Word32
  , ba_price           :: Word32
  , ba_timeStamp       :: N50.TimeStamp6
  } deriving (Eq, Ord, Show)

data Order = OBid BidAsk | OAsk BidAsk
           deriving (Eq, Ord, Show)

addToOrderBook :: Order -> OrderBook -> OrderBook
addToOrderBook (OBid o) ob = ob { ob_bids = o `Set.insert` ob_bids ob }
addToOrderBook (OAsk o) ob = ob { ob_asks = o `Set.insert` ob_asks ob }

deleteFromOrderBook :: Order -> OrderBook -> OrderBook
deleteFromOrderBook (OBid o) ob = ob { ob_bids = o `Set.delete` ob_bids ob }
deleteFromOrderBook (OAsk o) ob = ob { ob_asks = o `Set.delete` ob_asks ob }

lookupOrderInBook :: Word64 -> OrderBook -> Maybe Order
lookupOrderInBook refNum ob =
  let o = BidAsk refNum 0 0 (N50.T6 0)
  in case ( Set.lookupLE o (ob_asks ob)
          , Set.lookupLE o (ob_bids ob) ) of
       ( Just o', _       ) -> Just (OAsk o')
       ( Nothing, Just o' ) -> Just (OBid o')
       ( Nothing, Nothing ) -> Nothing

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
pOrder (N50.MAddOrder         x) ob = addToOrderBook (fromAddOrder x) ob
-- pOrder (N50.MAddOrderMPIDAttr x) s = Set.insert (N50.aoa_orderReferenceNumber x) s
-- pOrder (N50.MOrderExecuted    x) s = s
-- pOrder (N50.MOrderExecutedWithPrice x) s = s
-- pOrder (N50.MOrderCancel      x) s = Set.delete (N50.oc_orderReferenceNumber x) s
pOrder (N50.MOrderDelete      x) ob =
  let refNum = N50.od_orderReferenceNumber x
  in case lookupOrderInBook refNum ob of
       Nothing -> error $ "error: cannot find order " ++ show refNum
       Just o  -> deleteFromOrderBook o ob
-- pOrder (N50.MOrderReplace     x) s = N50.or_newOrderReferenceNumber x `Set.insert` Set.delete (N50.or_originalOrderReferenceNumber x) s
-- pOrder (N50.MTrade            x) s = s
-- pOrder (N50.MCrossTrade       x) s = s
-- pOrder (N50.MBrokenTrade      x) s = s
-- pOrder _                         s = s

--------------------------------------------------------------------------------

opts :: OA.Parser (IO ())
opts = OA.subparser
  (     OA.command "show"    (OA.info (showRecords <$> OA.argument OA.str OA.idm) (OA.progDesc "show all records in TARGET"))
  OA.<> OA.command "orders1" (OA.info (showOrders1 <$> OA.argument OA.str OA.idm) (OA.progDesc "show orders1 in TARGET"))
  OA.<> OA.command "orders2" (OA.info (showOrders2 <$> (read <$> OA.argument OA.str OA.idm) <*> OA.argument OA.str OA.idm) (OA.progDesc "show orders2 in TARGET"))
  OA.<> OA.command "stop"    (OA.info (pure (return ())) OA.idm) )

main :: IO ()
main = join $ OA.execParser (OA.info opts OA.idm)
