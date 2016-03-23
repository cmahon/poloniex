module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Text()
import System.Exit

import Poloniex.API

-----------------------------------------------------------------------------

main :: IO ()
main = getConfig >>= either (const exitFailure) run 
  
getConfig :: IO (Either String Config)
getConfig = return (Right ())

run :: Config -> IO ()
run cfg = do
  void . runPoloniex cfg $ do
    io =<< ticker
    io =<< volume
    io =<< orderBook (Just "BTC_NXT") (Just 10)
    io =<< tradeHistory (Just "BTC_NXT") Nothing Nothing
    io =<< chartData (Just "BTC_NXT") Nothing Nothing (Just 86400)
    io =<< currencies
  void . runSubscription $ do
    void $ sub "ticker"
    replicateM_ 5 (io =<< recv)

 where

  io :: (MonadIO m, Show a) => a -> m ()
  io t = liftIO $ print t >> putChar '\n' >> threadDelay 1000000