Fast radicle app state updates
==============================

.. date:: 2018-12-19
.. reviewers::

   @geigerzaehler
   @MeBrei
   @jkarni
   @cloudhead

Synopsis
---------

This RFC proposes full serialisation of the radicle interpreter state as a
solution to maintaining up-to-date views/projections of the state of a
long-running machine that is relevant to an app.

Motivation
----------

An *app* (e.g. the radicle issues CLI) is a program which reads data from a
radicle machine, displays it to the user, and facilitates ``send!`` ing inputs
to remote machines. An app often just depends on one or more *projections* of
the full radicle state. Currently, to compute this projection, an app has to run
the interpreter over all the inputs from the start of the machine, and then
submit an expression for evaluation.

If the app no-longer has the interpreter state in memory (e.g. computer was
restarted), it must run through all the inputs from the inception of the
machine, which is costly and slow.

Criteria
-------

*essential:*

- An app shouldn't need to process all inputs when it restarts, in order to
  compute the relevant state projection.

Proposal
----------

In order to start up and derive a projection without going through all the
inputs from the start, an app needs to be able to serialise *something* to disc.
That can be either the projection itself or the full interpreter state.

The most robust solution is to serialise the full interpreter state. This is
challenging because:

(1) The state has a high degree of structural sharing. This is because there are
    lots of embedded environments, and they are all very similar to one another
    (differ by the addition of a few bindings).

(2) The state is cyclic. Once source of cyclicity is recursive lambdas, but
    there might be others.

The advantages of serialising the full interpreter state are:

- The projection is guaranteed to be correct.

- It is likely that we will need to serialise the interpreter state for other
  reasons.

Drawbacks
----------

The main drawback is that the interpreter code is likely to be more complex, and
so more prone to bugs. It might also be less performant. It is conceivable that
the interpreter has two modes, one with serialisable state, and one that is
optimised for performance.

Alternatives
-------------

The alternative is to only serialise the projection of the state that is
relevant to the app. The question then arises of how to update this projection
given new inputs. The function which performs this update is likely to:

- Share a lot of logic with the code of the machine itself,

- Have no guarantee it will project the state correctly,

- Need to morph every time the semantics of the machine change.

Or it could derive the projections from the *outputs*, but this would require
acquiring (and trusting) the stream of outputs from some source.

With regards to the implementation, it may be possible to make use to
RefSerialize_.

Unresolved question
--------------------

Unclear how challenging the code changes to the interpreter are.

Implementation
--------------

In order to be able to serialise the state despite (1) and (2), the interpreter
implementation is changed to accommodate a new representation of environments:

- The interpreter state is augmented with ``sharedValues :: Map Hash Value``,
  mapping hashes of values to values, and ``sharedEnvs :: Map Hash Env``
  mappings hashes of environments to environments. These are global registries
  of values/envs which may be referenced in many environments.

- Currently environments are represented as ``Map Ident Value``. This is
  replaced with a new representation ``{ parent :: Maybe Hash, bindings :: Map
  Ident Hash }``. The ``bindings`` map identifiers to hashes which appear as
  keys in ``sharedValues``. The ``parent`` is an optional reference to an
  environment stored in ``sharedEnvs``. So a binding ``(i :: Ident, v ::
  Value)`` is now stored as a pair of bindings ``(i, hash v)`` (in ``bindings``
  directly, or recursively in one of the parent environments) and ``(hash v,
  v)`` in ``sharedValues``. When looking up the value associated to an
  identifier in an environment, first ``bindings`` is checked, and if the
  identifier is not found there, the ``parents`` are searched recursively.

- When evaluating a new scope (body of a lambda or module), the same base
  environment is used as the ``parent`` for all the environments created. So for
  example all the closures created while interpreting ``prelude/dict`` can have
  environments which share the same ``parent``. This avoids all these closures
  containing exactly the same bindings to primfns, etc. This involves
  maintaining the current ``parent`` environment and the definitions created in
  the current scope when evaluating such a scope.

References
-----------

RefSerialize_

.. _RefSerialize: https://hackage.haskell.org/package/RefSerialize
