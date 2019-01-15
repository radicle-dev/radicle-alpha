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

Proposal
---------

The radicle daemon is an executable which starts a server which
listens for HTTP requests on a specific port. It exposes a JSON API.

.. _endpoints:

**Endpoints:**

Using any ``/machines`` endpoint will trigger the daemon subscribing
and polling for activity on the machine, as described pubsub_. Any new
inputs received will be pinned.

- *Query* a machine, that is, run an expression against it.

  ``POST /machines/:machine_id/query``

  .. code-block:: json

     {"expression": "(get-state)"}

  Response:

  .. code-block:: json

     {"query_result": "{:apples 22 :oranges 42}"}

  The daemon will check for new inputs and then run the expression in
  the materialised machine state, sending the result back as a
  response.

- *Send* an input:
  
  ``POST /machines/:machine_id/send``

  .. code-block:: json

     {"expressions": ["(add-number 42)",
                      "(add-number 43)"]}

  First the daemon checks if it is the *writer* for this machine (see
  newMachine_). In this case (and if the input is valid), it will
  write it to IPFS, and then send out a message of type
  ``"new_input"`` on the machines' pubsub topic (with no nonce).

  If it is not the writer, the daemon will assume it is a *reader* for
  that machine. It will generate a random nonce, and send

  .. code-block:: json

    {"type": "input_request",
     "nonce": "abc123",
     "expression": "(add-number 42)"}
  
  to the IPFS pubsub topic associated to the machine and wait for a
  message with the same nonce:

  .. code-block:: json

    {"type": "new_input",
     "nonce": "abc123"}

  If no such message is received within a set time frame it will return
  an error response.
  
- .. _newMachine:

  *New* (IPFS) machine:
  
  ``POST /machines/new``

  Creates a new empty machines on IPFS and returns the resulting
  ``machine_id``. The daemon configures itself as the *writer* for
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

  When new inputs are detected, the materialised machine state is
  updated, ready for query requests.

- For all machines for which the daemon is the *writer*, it will
  subscribe to the machine's IPFS pubsub topic to listen for messages
  of type ``"input_request"``. When it receives such a message, it
  will check to see if the ``"expression"`` is valid. If it is, it
  will add it to the linked-list of inputs on IPFS, and publish the
  updated IPNS record. If this is successful, the daemon posts a
  message of type ``"new_input"`` with the same nonce to the
  machines's pubsub topic.
  
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

- In the future, Pubsub messages of type ``"new_input"`` should be
  signed, preferably with the private key paired with the IPNS ID.

- The initial version of the daemon will materialise all machines and
  pin all inputs. In the future there should be a mechanism for
  controlling the cache lifetime of a machine, and possibly polling
  settings.

Implementation
---------------

- Share as much code with ``radicle-server`` as possible.

References
-----------

- IPNS_

- IPFSPubsub_

.. _IPNS: https://docs.ipfs.io/guides/concepts/ipns/
.. _IPFSPubsub: https://blog.ipfs.io/25-pubsub/
