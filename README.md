# Poloniex

Haskell bindings to the [Poloniex](https://poloniex.com/) exchange API.

Early stage work in progress.

## To do

* [ ] Streamline service definitions with parameterised types
* [ ] Public services
  * [ ] Typed parameters
  * [ ] Typed responses
* [ ] Private trading services
* [ ] Websocket services (WAMP-based)
  * [X] Fork and upgrade haskell-wamp package
  * [X] Prototype
  * [ ] Ticker service
  * [ ] Orderbook service
  * [ ] Trollbox service
  * [ ] Non-unit return type for SubscriptionT
  * [ ] Auto-derivation of instances for SubscriptionT
* [ ] Remove unused packages from cabal file

## Setup

```bash
git clone https://github.com/cmahon/haskell-wamp
git clone https://github.com/cmahon/poloniex
cd poloniex
stack build
stack exec poloniex
```

## References/Sources

* [haskell-wamp](https://github.com/mulderr/haskell-wamp)
* [Poloniex API](https://poloniex.com/support/api/)
