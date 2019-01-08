Getting Started
==================

The easiest way to play with `radicle` is to use the online REPL, available at
the `radicle` website_.

.. _website: http://radicle.xyz

The online REPL is quite limited, however (and slow). So you will probably also
want the `radicle` executable.

Building from source
----------------------

In order to build from source, you'll need stack_:

.. _stack: https://docs.haskellstack.org/en/stable/README/

.. code-block:: bash

    curl -sSL https://get.haskellstack.org/ | sh

The centralized server also requires PostgreSQL libs. On Ubuntu, you can
`apt-get install libpq-dev libtinfo-dev`. On MacOS, you can `brew install postgresql`.

You can then download and build the latest source code for `radicle` from
http://git.oscoin.io/radicle:

.. code-block:: bash

    git clone https://github.com/oscoin/radicle.git
    cd radicle
    stack build
    stack install

You can test that `radicle` was properly installed by running:

.. code-block:: bash

    radicle --help

Note that the `radicle` executable always takes a file as an argument. If you
want to run it interactively (i.e., as a REPL), you'll need to pass it the REPL
file (included in the repository):


.. code-block:: bash

    radicle rad/repl.rad
