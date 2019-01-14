Radicle daemon
===============

.. date:: 2019-01-09

Synopsis
---------

This RFC gives a rough spec of the *radicle daemon*, a long-running
background process which materialises the state of remote IPFS
machines on the users PC, and writes to IPFS machines if the user
happens to be the owner.

Motivation
-----------

- Currently to run a query against the state of some remote chain, all
  the inputs are downloaded and the state re-materialised from
  scratch. This is too slow.

- Currently collaborators to a machine (= non-owners) must communicate
  inputs to the owner for inclusion into the machine. This daemon
  would listen for inputs on an IPFS pubsub channel.

- The daemon also allows apps not written in radicle or Haskell to
  have access to a radicle interpreter by maintaining *local
  machines*.

Proposal
---------

The radicle daemon is an executable which starts a server which
listens for HTTP requests on a specific port. It exposes a JSON API.

The two main endpoints are:

- Query a machine, that is, run an expression against it.

  ``POST /:machine_url/query``

  .. code-block:: json

    {"expression": "(get-state)",
     "keep_input": 259200,
     "keep_polling": 3600,
     "poll_period": 30}

  Results in something like:

  .. code-block:: json

    {"result": "{:apples 22 :oranges 42}",
     "last_input_id": 55}

  The daemon works out if it must query a remote or local machine
  based on the protocol (``radicle-local://`` versus ``ipfs://``). In
  the local case it will store the input expressions in a file. For
  remote IPFS machines it will fetch and pin the inputs by sending
  queries to the IPFS daemon which it assumes is running.

  The fields ``keep_input``, ``keep_polling``, ``poll_period`` are
  optional.

  - ``keep_input`` controls how long (seconds) the daemon will cache
    the inputs for. In the case of a local machine this controls how
    long it will keep a log-file of all the expressions for. In the
    case of an IPFS machine, it controls for how long the daemon will
    pin the inputs.

  - ``keep_polling`` and ``poll_period`` control how long (seconds) it
    should keep polling for new inputs for (after the last
    interaction) and the amount of time between each poll.

- Send an input:
  ``POST /:machine_url/send``

  .. code-block:: json

    {"expression": "(inc)",
     "keep_input": 259200,
     ...}

  For local machines this adds an input to the machine directly.

  For remote IPFS machines this will first check a radicle config file
  for private keys indicating that this PC is the *writer* for this
  IPFS chain. In this case (and if the input is valid), it will write
  it to IPFS.

  If such a config is not found, the daemon will assume it is a
  *reader* for that machine, and it will send the input to the IPFS
  pubsub topic associated to the machine and wait for an ``ACK``. If
  no ``ACK`` is received within a reasonable timeframe, it will return
  an error stating the machine owner is offline.

Additionally, for all machines for which the daemon is the *writer*,
it will subscribe to the machine's IPFS pubsub topic to listen for
input suggestions from contributors. When it receives such a message,
it will check to see if the input is valid. If it is, it will write it
to the IPFS and send back an ``ACK``.
  
Drawbacks
----------

- An extra executable to maintain.

- More installation steps for users.

- Depends on the IPFS daemon.

Alternatives
-------------

- Just use state serialisation, but this currently looks like a complex refactor
  of the radicle codebase.

Unresolved question
--------------------

- The daemon might be used by multiple apps to access data about the same remote
  machine. In this case something should prevent one app disabling the
  cache/polling settings of the other.

- How are ``ACK`` sent back? Do we use throwaway topics?
  
- It might be possible to replace polling with writing and subscribing
  to IPFS pubsub topics.

Implementation
---------------

- Share as much code with ``radicle-server`` as possible.

- Use files instead of postgres for persistence.

References
-----------

N/A
