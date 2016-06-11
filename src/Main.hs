module Main where

import Control.Monad

import Data.Word

import qualified Data.Binary            as S
import qualified Data.Binary.Get        as S
import qualified Data.ByteString.Char8  as BC
import qualified Data.ByteString.Lazy   as L

import qualified Data.Set               as Set

import qualified Codec.Compression.GZip as GZ

import qualified Options.Applicative as OA

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

showRecords :: FilePath -> IO ()
showRecords fileName =
  mapM_ printMessage =<< (decodeToList . GZ.decompress <$> L.readFile fileName)
  
printMessage :: Message -> IO ()
printMessage msg@(Message len con) = do
  -- print msg
  case S.decode con :: N50.Message of
    N50.MOther _ _ -> return ()
    m              -> print m

showOrders :: FilePath -> IO ()
showOrders fileName =
  print . onlyIncreases . map Set.size =<< (consolidate Set.empty . decodeToList . GZ.decompress <$> L.readFile fileName)

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
  
opts :: OA.Parser (IO ())
opts = OA.subparser
  (     OA.command "show"   (OA.info (showRecords <$> OA.argument OA.str OA.idm) (OA.progDesc "show all records in TARGET"))
  OA.<> OA.command "orders" (OA.info (showOrders  <$> OA.argument OA.str OA.idm) (OA.progDesc "show all records in TARGET"))
  OA.<> OA.command "stop"   (OA.info (pure (return ())) OA.idm) )

main :: IO ()
main = join $ OA.execParser (OA.info opts OA.idm)
