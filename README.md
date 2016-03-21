# Poloniex

Haskell bindings to the [Poloniex](https://poloniex.com/support/api/) API. 

Early stage work in progress.

## To do

* [ ] Streamline service definitions with parameterised types
* [ ] Public services
  * [ ] Typed parameters
  * [ ] Typed responses
* [ ] Private trading services
* [ ] Websocket services (WAMP-based)
* [ ] Remove unused packages from cabal file

## Notes

Successfully invoked websockets ticker service using a modified version of [haskell-wamp](https://github.com/mulderr/haskell-wamp). Changes were made to implement secure sockets and facilitate build on GHC 7.10.x.

```bash
Running test...
✓ Session established: SessId 7269015193767049
✓ Subscribed: Subscription SubId 7732592049648916 TopicUri "ticker"
Event (SubId 7732592049648916) (PubId 2598292919943376) (Details (fromList [])) (Arguments [String "BTC_XCP",String "0.00338707",String "0.00359674",String "0.00338708",String "-0.33588816",String "290.74817288",String "73175.81063986",Number 0.0,String "0.00519998",String "0.00332300"]) (ArgumentsKw (fromList []))
Event (SubId 7732592049648916) (PubId 168655739539051) (Details (fromList [])) (Arguments [String "BTC_ETH",String "0.02485401",String "0.02485500",String "0.02485401",String "-0.02883674",String "22703.76169001",String "923455.15485684",Number 0.0,String "0.02660000",String "0.02300000"]) (ArgumentsKw (fromList []))
✓ Unsubscribed
✓ Published
```