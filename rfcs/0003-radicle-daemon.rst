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

.. _endpoints:

**Endpoints:**

Using any ``/machines`` endpoint will trigger the daemon subscribing
and polling for activity on the machine, as described pubsub_.

- *Query* a machine, that is, run an expression against it.

  ``POST /machines/:machine_id/query "(get-state)"``

  Results in something like: ``"{:apples 22 :oranges 42}"``.

  The daemon uses the IPNS link to check for new inputs. It also asks
  the IPFS daemon to pin all the inputs.

- *Send* an input:
  ``POST /machines/:machine_id/send "(inc)"``

  First the daemon checks with the IPFS daemon to see if it is the
  *writer* for this machine. In this case (and if the input is valid),
  it will write it to IPFS.

  If it is not the writer, the daemon will assume it is a *reader* for
  that machine it will generate a random nonce, and it will send

  .. code-block:: json

    {"type": "input_request",
     "nonce": "abc123",
     "expression": "(add-number 42)"}
  
  to the IPFS pubsub topic associated to the machine and wait for an:

  .. code-block:: json

    {"type": "new_input",
     "nonce": "abc123"
     "expression": "(add-number 42)"}

  If no such message is received within a set timeframe it will return
  an error reponse.

- *New* (IPFS) machine:
  ``POST /machines/new``

  Creates a new empty machines on IPFS and returns the resulting
  ``machine_id``. The daemon will now assume it is the *writer* for
  this machine.

.. _pubsub:

**Pubsub and polling:**

- For all machines for which the daemon is a *reader*, it will poll
  IPNS for new inputs at a low frequency. Furthermore, it will
  subscribe to the machine's IPFS pubsub topic and listen for messages
  with type ``"new_input"``. When such a message is received, it will
  start polling for new inputs at a high frequency. If none are
  detected after a certain time period the daemon will assume it was a
  late pubsub message and return to low-frequency polling.

- For all machines for which the daemon is the *writer*, it will
  subscribe to the machine's IPFS pubsub topic to listen for messages
  of type ``"input_request"``. When it receives such a message, it
  will check to see if the ``"expression"`` is valid. If it is, it
  will write it to IPFS and update the IPNS link and post a message of
  type ``"new_input"`` with the same nonce and expression.
  
Drawbacks
----------

- An extra executable to maintain.

- More installation steps for users.

- Depends on the IPFS daemon.

Alternatives
-------------

N/A

Unresolved question
--------------------

- The daemon might be used by multiple apps to access data about the same remote
  machine. In this case something should prevent one app disabling the
  cache/polling settings of the other.

Implementation
---------------

- Share as much code with ``radicle-server`` as possible.

References
-----------

IPNS_
IPFSPubsub_

.. _IPNS: https://docs.ipfs.io/guides/concepts/ipns/
.. _IPFSPubsub: https://blog.ipfs.io/25-pubsub/
