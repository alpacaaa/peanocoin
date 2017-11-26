
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
pulp build
```

### Running

Spawn a node on a specific port (default `3000`).

```bash
node spawn-peanos.js --port 4000
```

### Testing

Unit tests

```bash
pulp test
```

Integration tests (requires `go` to be installed)
```bash
go run yolo.go
```

This will spin up 3 nodes and have them communicate in various ways to ensure they are all receiving blocks/transactions and updating their blockchain properly.