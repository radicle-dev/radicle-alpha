Radicle daemon
===============

.. date:: 2019-01-09

Synopsis
---------

This RFC gives a rough spec of the *radicle daemon*, a long-running background
process which replicates remote machines on the users PC.

Motivation
-----------

Currently to run a query against the state of some remote chain, all the inputs
are downloaded and the state re-materialised from scratch. This is too slow.

The daemon also allows apps not written in radicle or Haskell to have access to
a radicle interpreter.

Proposal
---------

The radicle daemon is an executable which starts a server which listens for HTTP
requests on a specific port. The API uses JSON.

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

  The daemon works out if it must query a remote or local machine based on the
  existence of a protocol (e.g. ``http://`` or ``ipfs://``). If there is no
  protocol then the query refers to a locally maintained machine. Otherwise it
  will fetch inputs from a remote machine using the correct protocol (but only
  ``http`` is supported initially).

  The fields ``keep_input``, ``keep_polling``, ``poll_period`` are optional.
  ``keep_input`` controls how long (seconds) the daemon will cache the inputs
  for. ``keep_polling`` and ``poll_period`` control how long (seconds) it should
  keep polling for new inputs for (after the last interaction) and the amount of
  time between each poll.

- Get the list of inputs of a remote machine:

  ``GET /:machined_url/inputs/:id``

  Returns all the inputs since the input with ID ``id``. Optionally accepts the
  same json caching/polling configuration json as above.

- Send an input:
  ``POST /:machine_url/send "(inc)"``

  For local machines this adds an input to the machine directly. For remote
  machines this attempts to ``send`` the expression.

Drawbacks
----------

- An extra executable to maintain.

- More installation steps for users.

Alternatives
-------------

- Just use state serialisation, but this currently looks like a complex refactor
  of the radicle codebase.

Unresolved question
--------------------

N/A

Implementation
---------------

- Share as much code with ``radicle-server`` as possible.

- Use files instead of postgres for persistence.

References
-----------

N/A
