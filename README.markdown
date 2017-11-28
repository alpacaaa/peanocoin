
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

Node will be reachable at `http://localhost:4000`

You can spawn multiple nodes on the same machine.



### Known issues

Things I need to fix.

- [ ] Reward is static, maybe it could decrease over time (not that big of a deal).
- [ ] Node discovery is still a bit rusty. I'd like peers to be added to a node's peers list whenever a request gets through, not just when explicitely requesting `/peers`.



### Usage

Peanocoin is built to be simple to use, so you can interact with any node directly from your browser.

All requests are sent via `GET`.

Nodes start with a balance of zero coins, meaning that coins aren't created out of thin air.

To get some coins, you have to mine blocks. Reward is currently set at `100` coins for the miner. So when you boot a new node, either `/mine-block` or `/transfer` some coins from another node.

Take a look at the integration tests if you want to see how nodes are booted up and how they interact over the network.



### API

##### `/`

Gives you an overview of the node state.

- `address` This is your node public address (ie. the address other peers will use to transfer coins to you).

- `blockchain` Contains all the blocks known by this node.

- `memPool` Transactions that are yet to be mined are held in the mempool.

- `peers` Known peers (other nodes)


##### `/boot?ip=other-node-ip`

A new node that wants to join the network (that is, any node that isn't the first to be spun up) should boot pinging a known peer. So, assuming there's a node running at `http://localhost:3000` and the new node willing to join it is listening at `http://localhost:4444` you would send this request to the latter:

```
http://localhost:4444/boot?ip=http://localhost:3000
```

This will requests all known peers and try to rebuild the blockchain up to the last known block.



##### `/ledger`

Ledger status. Basically answers the question "How many coins are owned by each node?"


##### `/transfer/:other_address/:amount`

Transfer `:amount` coins to `:other_address`. Will fail if balance is insufficient.

Note that the ledger won't reflect the transfer until the transaction is mined in a block.
The transaction is broadcasted to known peers and included in the mempool.


##### `/mine-block`

Mines a block including valid transactions present in the mempool. (you can still mine a block even with an empty mempool).

The freshly mined block is sent over the network so that it gets included in other nodes blockchains.

Mining difficulty is currently set to 5 leading zeros (that is, the hash of a valid block should start with `00000` for it to be included in the blockchain). This is enough to make PoW noticeable (should take a few seconds to mine a block).



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
