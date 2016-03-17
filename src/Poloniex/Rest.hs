{-# LANGUAGE KindSignatures #-}

module Poloniex.Rest where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Data.Aeson.Types
import           Data.Proxy
import           Data.Text (Text)
import           Servant.API
import           Servant.Client

-----------------------------------------------------------------------------

type Host = String
type Port = Int

restHost :: Host
restHost = "poloniex.com"

restPort :: Port
restPort = 443

-----------------------------------------------------------------------------

type ServantT  = EitherT ServantError IO
type Config    = ()
type PoloniexT = ReaderT Config ServantT

runPoloniex :: Config -> PoloniexT a -> IO (Either ServantError a)
runPoloniex cfg = runEitherT . flip runReaderT cfg

-----------------------------------------------------------------------------

type PoloniexAPI           = TickerService
                        :<|> VolumeService
                        :<|> OrderBookService
                        :<|> TradeHistoryService
                        :<|> ChartDataService
                        :<|> CurrenciesService

type TickerService         = Public
                          :> QueryParam "command" CommandTicker
                          :> Get '[JSON] Value
type VolumeService         = Public
                          :> QueryParam "command" CommandVolume
                          :> Get '[JSON] Value
type OrderBookService      = Public
                          :> QueryParam "command" CommandOrderBook
                          :> QueryParam "currencyPair" Text
                          :> QueryParam "depth" Int
                          :> Get '[JSON] Value
type TradeHistoryService   = Public
                          :> QueryParam "command" CommandTradeHistory
                          :> QueryParam "currencyPair" Text
                          :> QueryParam "start" Int
                          :> QueryParam "end" Int
                          :> Get '[JSON] Value
type ChartDataService      = Public
                          :> QueryParam "command" CommandChartData
                          :> QueryParam "currencyPair" Text
                          :> QueryParam "start" Int
                          :> QueryParam "end" Int
                          :> QueryParam "period" Int
                          :> Get '[JSON] Value
type CurrenciesService     = Public
                          :> QueryParam "command" CommandCurrencies
                          :> Get '[JSON] Value

-- type CommandParam a        = QueryParam "command" a
-- type OrderBookParams       = QueryParam "currencyPair" Text :> QueryParam "depth" Int
-- type TradeHistoryParams    = QueryParam "currencyPair" Text :> QueryParam "start" Int :> QueryParam "end" Int
-- type ChartDataParams       = QueryParam "currencyPair" Text :> QueryParam "start" Int :> QueryParam "end" Int :> QueryParam "period" Int

-----------------------------------------------------------------------------

type Public                = "public"

-----------------------------------------------------------------------------

data CommandTicker       = CommandTicker       deriving Show
data CommandVolume       = CommandVolume       deriving Show
data CommandOrderBook    = CommandOrderBook    deriving Show
data CommandTradeHistory = CommandTradeHistory deriving Show
data CommandChartData    = CommandChartData    deriving Show
data CommandCurrencies   = CommandCurrencies   deriving Show

instance ToText CommandTicker where toText _       = "returnTicker"
instance ToText CommandVolume where toText _       = "return24hVolume"
instance ToText CommandOrderBook where toText _    = "returnOrderBook"
instance ToText CommandTradeHistory where toText _ = "returnTradeHistory"
instance ToText CommandChartData where toText _    = "returnChartData"
instance ToText CommandCurrencies where toText _   = "returnCurrencies"

-----------------------------------------------------------------------------

type PublicService a       = Public
                             :> a
                             :> Get '[JSON] Value

-----------------------------------------------------------------------------

api :: Proxy PoloniexAPI
api = Proxy

-----------------------------------------------------------------------------

ticker_         :: Maybe CommandTicker -> ServantT Value
volume_         :: Maybe CommandVolume -> ServantT Value
orderBook_      :: Maybe CommandOrderBook -> Maybe Text -> Maybe Int -> ServantT Value
tradeHistory_   :: Maybe CommandTradeHistory -> Maybe Text -> Maybe Int -> Maybe Int -> ServantT Value
chartData_      :: Maybe CommandChartData -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int -> ServantT Value
currencies_     :: Maybe CommandCurrencies -> ServantT Value

ticker_      
  :<|> volume_      
  :<|> orderBook_   
  :<|> tradeHistory_
  :<|> chartData_   
  :<|> currencies_   = client api (BaseUrl Https restHost restPort)

-----------------------------------------------------------------------------

ticker :: PoloniexT Value
ticker = lift $ ticker_ $ Just CommandTicker

volume :: PoloniexT Value
volume = lift $ volume_ $ Just CommandVolume

orderBook :: Maybe Text -> Maybe Int -> PoloniexT Value
orderBook c d = lift $ orderBook_ (Just CommandOrderBook) c d

tradeHistory :: Maybe Text -> Maybe Int -> Maybe Int -> PoloniexT Value
tradeHistory c s e = lift $ tradeHistory_ (Just CommandTradeHistory) c s e

chartData :: Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int -> PoloniexT Value
chartData c s e p = lift $ chartData_ (Just CommandChartData) c s e p

currencies :: PoloniexT Value
currencies = lift $ currencies_ $ Just CommandCurrencies
