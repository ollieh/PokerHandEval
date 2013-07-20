module PokerHandEval (evalHand, Result) where

import qualified Data.ByteString.Lazy as BS
import Data.Bits (shift, (.&.))
import Data.Binary.Get
import Data.Int
import Control.Monad

data Result = Result
    { handType :: Integer
    , handRank :: Integer
    , value :: Integer
    , handName :: String
    } deriving (Show)

evalHand :: [Integer] -> Maybe (IO (Result))
evalHand cs
    | (length cs == 7) = Just $ eval cs
    | otherwise = Nothing

eval :: [Integer] -> IO (Result)
eval cs = do
    res <- foldM (\ x y -> evalCard (x + y)) 53 cs
    return (Result
        (res `shift` (-12))
        (res .&. 0x00000fff)
        (res)
        (handTypeToName $ (shift (fromIntegral res) (-12))))

handTypeToName :: Int -> String
handTypeToName n = handtypes !! n
    where handtypes = ["invalid hand","high card","one pair","two pairs","three of a kind","straight","flush","full house","four of a kind","straight flush"]

evalCard :: Integer -> IO (Integer)
evalCard n = do
    handranks <- BS.readFile "HandRanks.dat"
    let output = toInteger $ runGet getWord32le $ BS.drop ((fromInteger n)*4) handranks
    return output
