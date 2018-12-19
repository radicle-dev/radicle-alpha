Fast, incremental radicle app state
===================================

.. date:: 2018-12-19
.. reviewers::

   @geigerzaehler
   @MeBrei
   @jkarni

Synopsis
---------

This RFC proposes full serialisation of the radicle interpreter state as a
solutions to maintaining up-to-date views/projections of the relevant state of a
long-running machine. For facilitating deriving these projections, it proposes
that machines outputs are write-commands for a particular database.

Motivation
----------

An *app* (e.g. the radicle issues CLI) is a program which reads data from a
radicle machine, to display to the user, and facilities ``send!``ing inputs to
remote machines. An app often just depends on one of more *projections* of the
full radicle state. Currently, to compute this projection, an app has to run the
interpreter over all the inputs from the start of the machine, and then submit
an expression for evaluation.

When new inputs are discovered, there is currently no efficient way to update
the projection: the interpreter must be moved forward over the new inputs and a
full query for the new state performed. Furthermore, if the app no-longer has
the interpreter state in memory (e.g. computer was restarted), it must run
through all the inputs from the inception of the machine, which is costly and
slow.

Criteria
-------

*essential:*

- An app shouldn't need to process all inputs when it restarts, in order to
  compute the relevant state projection.

- An app should be able to incrementally update the projection given new machine
  inputs.

Proposal
----------

In order to start up and derive a projection without going through all the
inputs from the start an app needs to be able to serialise *something* to disc.
That can be either the projection itself or the full radicle state.

The most robust solution is to serialise the full interpreter state. This
presents its own challenges:

(a) The state has a high degree of structural sharing. This is because there are
    lots of embedded environments, and they are all very similar to one another
    (differ by the addition of a few bindings).

(b) The state is cyclic. Once source of cyclicity is recursive lambdas, but
    there might be others.

The advantages of serialising the full interpreter state are:

- The projection is guaranteed to be correct.

- It is likely that we will need to serialise the interpreter state for other
  reasons.

We propose two stages:

**Stage 1: recreating radicle state**

In order to be able to serialise the state despite (a) and (b), the interpreter
code is changed to accommodate a new representation of environments:

- Environments are maps from identifiers to *hashes* of values.

- The hashes are treated as pointers into a global values map, from hashes to
  values.

- Environments are hierarchical, so that the sharing is explicit.

**Stage 2: incrementally updated views**

.. _stage2:

For incrementally updating the projections/view, we propose that machines
(optionally) standardise their outputs as *write-commands* for a database.
Updating a view is then as simple as:

- Updating the database with the command,

- Re-running the query which produces the view.

For the choice of DB I would propose: Datascript_, a datalog-like tripe-store.
The advantages are:

- It fits the radicle datamodel perfectly (since it's based on that of Clojure).

- The writes and queries are just radicle data, so this is perfect as a radicle
  output.


Using a "reactive" database would allow the projections to be recomputed even
more efficiently, though this is probably not necessary:

- RxDB_

- Clara_, an implementation of Rete_, but see FactUI_ for an explanation of how
  it would be used in this case.

Drawbacks
----------

The main drawback is that the interpreter code is likely to be more complex, and
so more prone to bugs. It might also be less performant.

Alternatives
-------------

The alternative is to only serialise the projection of the state that is
relevant to the app. The question then arises of how to update this projection
given new inputs:

The function which performs this update is likely to:

- Share a lot of logic with the code of the machine itself,

- Have no guarantee it will project the state correctly,

- Need to morph everytime the semantics of the machine change.

Or it could derive the projections from the *outputs* (as described in stage2_),
but this would require acquiring (and trusting) the stream of outputs from some
source.

Unresolved question
--------------------

- Unclear how challenging the changes to the interpreter for stage 1 are.

- Unclear which DB to use in stage 2. Since it is likely machines will also want
  to take advantage of the DB, and that we encourage apps to be written in
  radicle, the DB features should be included as part of the radicle package. We
  could either include an off-the-shelf DB which fits the radicle datamodel
  (e.g. Datascript_), or build our own.

Implementation
--------------

First implement stage 1, since this is a priority. Stage 2 can be left for a lot
later.

References
-----------

RefSerialize_
Datascript_
RxDB_
Rete_
Clara_
FactUI_

.. _RefSerialize: https://hackage.haskell.org/package/RefSerialize
.. _Datascript: https://github.com/tonsky/datascript
.. _RxDB: https://github.com/pubkey/rxdb
.. _Rete: https://en.wikipedia.org/wiki/Rete_algorithm
.. _Clara: http://www.clara-rules.org/
.. _FactUI: https://github.com/arachne-framework/factui
