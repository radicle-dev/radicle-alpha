Radicle Peer-to-peer Using `hypercore`
======================================

This document provides a high-level overview of how to build a P2P peer system
to distribute and collaborate on Radicle chains using the
[`hypercore`][hypercore] and [`discovery-swarm`][discovery-swarm] libraries.

[hypercore]: https://github.com/mafintosh/hypercore
[discovery-swarm]: https://github.com/mafintosh/discovery-swarm

### Status of This Document

This document is only for informational purposes and to summarize our research
into `hypercore`. We decided not to build Radicle P2P on `hypercore`. You can
see the discussion about this document in
[PR#203](https://github.com/oscoin/radicle/pull/203).


Hypercore
---------

`hypercore` is a library to distribute cryptographically secure append-only logs
called *feeds*. A feed consists of a list of *entries* which may be JSON objects
or binary blobs. A feed is signed with a private key. The hash of the
corresponding public key is called the *discovery key* and acts as an identifier
for the feed.

Any user who knows the location (for example the IP and port) of a feed can read
any entry in the feed, verify the integrity of any entry, listen to new entries,
and replicate the feed.

We say a user *hosts a feed* if the user makes it available to the world. If a
users hosts a feed the user is either the owner of that feed or she replicates
it.

`hypercore` supports feed subscriptions. A user can subscribe to a feed and will
be notified if the owner publishes a new entry.

Every feed a user owns or replicates is stored locally and updated automatically
if a user goes online.

`discovery-swarm` is a library to discover hosts for hypercore feeds and
announce the hosting of a feed. It uses the Bittorrent DHT and centralised DNS
for global discovery as well as Multicast DNS for local discovery.


Single-writer Chains
--------------------

*Single-writer chains* are Radicle chains that only the owner of a specific
private key can write to.

We use hypercore feeds to represent single-writer chains. Each entry in the feed
is a Radicle expression. Single-writer chains are identified by the discovery
key of their feed.

To read a Radicle chain a user needs the discovery key of a feed. With the
discovery key the user uses hyperdiscovery to find someone who replicates the
feed. (This may be the owner.) The user then reads all entries in the feed with
hypercore and evaluates them with radicle.

To write an expression to a Radicle chain a user publishes a new entry to the
`hypercore` feed.


Multi-writer Chains
-------------------

*Multi-writer chains* are Radicle chains that the owners of multiple distinct
private keys can write to.

We implement a multi-writer chain by using set of feeds owned by different
users. Every entry in these feeds is a Radicle expression and a link to another
entry in any of feeds. Every entry in a feed now determines a linked list of
Radicle expression (assuming the feed entries and links are well formed and
valid) and thus a Radicle chain.

To derive a Radicle chain from a set of feeds we need an algorithm that takes
the heads of all the feeds of a multi-writer chain and outputs either a chain of
Radicle expressions or signals that the chain is invalid.

The simplest such algorithm is the following: We accept only the situation where
there is a feed with head entry `e` so that the head entries of all other feeds
are ancestors of `e`. In that case `e` is considered the head of the chain.
Otherwise the chain is invalid.


**TODO** Properties of this scheme

**TODO** Addressing entries


Offline First and Replication
-----------------------------

**TODO** flesh this out

Hypercore stores every feed a user is interested locally.

Hypercore allows one user to replicate the feed of another user. Updates are
propagated automatically.

Users can fetch a feed from a user replicating it if the owner is not online.

Implementation
--------------

`hypercore` and `discovery-swarm` are only available as JavaScript libraries. To
build Radicle P2P on top of hypercore we would need to implement the core
functionality of a Radicle P2P node in JavaScript. (Typescript and Purescript
are alternative languages since they can use JavaScript transparently.) This
node would expose an API that would allow the Radicle REPL and other tools to
submit and get expressions.

The node will be accompanied by a control CLI that allows users to create
chains, replicate chains, etc.

Security
--------

* Feeds can be censored. If the owner goes offline and all users that replicate
  the feed conspire to drop part of the head of the feed. If one user chooses to
  replicate the actual head of the feed then everyone else will see that head,
  too.
* Feeds are unforgeable. Anybody with the public key of a feed can verify
  whether a given entry was published to the feed by its owner.

Open Questions
--------------

* How robust and secure are `hypercore` and `discovery-swarm`?
* How many feeds can a node host? From the examples and the code it looks
  like a node can host one feed per port.
