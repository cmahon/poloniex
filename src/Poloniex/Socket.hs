module Poloniex.Socket where

import           Control.Concurrent.MVar
import           Control.Monad.Reader
import           Network.Wamp.Client
import           Network.Wamp.Connection hiding (Session (..))
import           Network.Wamp.Messages
import           Network.Wamp.State
import           Network.Wamp.Types
import           Prelude hiding (lookup)

import           Poloniex.Types

-----------------------------------------------------------------------------

wsHost :: Host
wsHost = "api.poloniex.com"

wsPort :: Port
wsPort = 443

-----------------------------------------------------------------------------

newtype SubscriptionT a = SubscriptionT 
  { unSubscriptionT :: ReaderT Session IO a}
  deriving Functor

instance Applicative SubscriptionT where
  pure = SubscriptionT . pure
  (SubscriptionT f) <*> (SubscriptionT x) = SubscriptionT (f <*> x)

instance Monad SubscriptionT where
  return = SubscriptionT . return
  SubscriptionT x >>= f = SubscriptionT (x >>= unSubscriptionT . f)

instance MonadIO SubscriptionT where
  liftIO = SubscriptionT . liftIO

instance MonadReader Session SubscriptionT where
  ask = SubscriptionT ask
  local f = SubscriptionT . local f . unSubscriptionT

-----------------------------------------------------------------------------

runSubscription :: SubscriptionT () -> IO ()
runSubscription =
  runClientWebSocket True "realm1" wsHost (fromIntegral wsPort) "/" 
    . runReaderT 
      . unSubscriptionT

-----------------------------------------------------------------------------

sub :: TopicUri -> SubscriptionT (Result Subscription)
sub feed = SubscriptionT $ do
  session <- ask
  liftIO $ subscribe session feed (Options dict) voidHandler

unsub :: Subscription -> SubscriptionT (Result Bool)
unsub subscription = SubscriptionT $ do
  session <- ask
  liftIO $ unsubscribe session subscription

recv :: SubscriptionT Message
recv = SubscriptionT $ do 
  session <- ask
  msg <- liftIO $ receiveMessage (sessionConnection session)
  liftIO $ messageHandler session msg
  return msg

-----------------------------------------------------------------------------

displayHandler :: Handler
displayHandler args _ _ = putStrLn $ show args

voidHandler :: Handler
voidHandler _ _ _ = return ()

messageHandler :: Session -> Message -> IO ()
messageHandler session msg = do
  case msg of
    Subscribed reqId subId -> do
      mr <- lookup (sessionSubscribeRequests session) reqId
      case mr of
        Nothing -> putStrLn $ "Unsolicited message: " ++ show msg
        Just (SubscribeRequest m _ topicUri handler) -> do
          let s = Subscription subId topicUri handler (Options dict)
          delete (sessionSubscribeRequests session) reqId
          insertSubscription (sessionSubscriptions session) s
          putMVar m (Right s)

    Unsubscribed reqId -> do
      mr <- lookup (sessionUnsubscribeRequests session) reqId
      case mr of
        Nothing -> putStrLn $ "Unsolicited message: " ++ show msg
        Just (UnsubscribeRequest m _ subId) -> do
          delete (sessionUnsubscribeRequests session) reqId
          deleteSubscription (sessionSubscriptions session) subId
          putMVar m (Right True)

    _ -> return ()



