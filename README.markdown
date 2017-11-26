
# Peanocoin

A PoC criptocurrency written in Purescript. Inspired by [nanocoin](https://github.com/tdietert/nanocoin).


### Project goals

- Learn Purescript
- Learn how to structure an app in Purescript
- Learn what a blockchain is and how it works
- Implement simple cryptocurrency
- P2P protocol on top of HTTP
- In memory


### Setup

```bash
bower install --dev
npm install --dev
```

### Testing

Unit tests

```bash
pulp test
```

Integration tests (requires `go` to installed)
```bash
go run yolo.go
```

This will spin up 3 nodes and have them communicate in various ways to ensure they are all receiving blocks/transactions and updating their blockchain properly.