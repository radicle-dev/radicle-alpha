Radicle Reference
=================

These are the functions that are available in a new radicle chain after
the prelude has been loaded.

Basics
------

Basic function used for checking equality, determining the type of a
value, etc.

``eq?``
~~~~~~~

Checks if two values are equal.

``not``
~~~~~~~

True if 'arg' is #f, false otherwise.

``and``
~~~~~~~

Returns 'arg2' if 'arg1' is not #f, otherwise returns 'arg1'

``or``
~~~~~~

Returns 'arg1' if 'arg1' is not #f, otherwise returns 'arg2'

``all``
~~~~~~~

Checks that all the items of a list are truthy.

``some``
~~~~~~~~

Checks that there is a least one truthy value in a list.

``show``
~~~~~~~~

Returns a string representing the argument value.

``string-append``
~~~~~~~~~~~~~~~~~

Concatenates a variable number of string arguments. If one of the
arguments isn't a string then an exception is thrown.

``string-length``
~~~~~~~~~~~~~~~~~

Returns the length of a string.

``apply``
~~~~~~~~~

Calls the first argument (a function) using as arguments the elements of
the the second argument (a list).

``type``
~~~~~~~~

Returns a keyword representing the type of the argument; one of:
``:atom``, ``:keyword``, ``:string``, ``:number``, ``:boolean``,
``:list``, ``:vector``, ``:function``, ``:dict``, ``:ref``,
``:function``.

``atom?``
~~~~~~~~~

Checks if the argument is a atom.

``boolean?``
~~~~~~~~~~~~

Checks if the argument is a boolean.

``string?``
~~~~~~~~~~~

Checks if the argument is a string.

``number?``
~~~~~~~~~~~

Checks if the argument is a number.

``keyword?``
~~~~~~~~~~~~

Checks if the argument is a keyword.

``list?``
~~~~~~~~~

Checks if the argument is a list.

``dict?``
~~~~~~~~~

Checks if the argument is a dict.

``read``
~~~~~~~~

Parses a string into a radicle value. Does not evaluate the value.

``read-many``
~~~~~~~~~~~~~

Parses a string into a vector of radicle values. Does not evaluate the
values.

``throw``
~~~~~~~~~

Throws an exception. The first argument should be an atom used as a
label for the exception, the second can be any value.

``Y``
~~~~~

A combinator used to create recursive functions of one variable.

``Y2``
~~~~~~

A combinator used to create recursive functions of two variables.

``to-json``
~~~~~~~~~~~

Returns a JSON formatted string representing the input value.

``uuid?``
~~~~~~~~~

Checks if a string has the format of a UUID.

``make-counter``
~~~~~~~~~~~~~~~~

Creates a stateful counter. Returns a dict with two keys: the function
at ``:next-will-be`` will return the next number (without incrementing
it), while the function at ``:next`` increments the number and returns
it.

``public-key?``
~~~~~~~~~~~~~~~

Checks if a value represents a valid public key.

Numerical functions
-------------------

Operations on numbers.

``+``
~~~~~

Adds two numbers together.

``*``
~~~~~

Multiplies two numbers together.

``-``
~~~~~

Substracts one number from another.

``/``
~~~~~

Divides one number by another. Throws an exception if the second
argument is 0.

``<``
~~~~~

Checks if a number is strictly less than another.

``>``
~~~~~

Checks if a number is strictly greater than another.

``integral?``
~~~~~~~~~~~~~

Checks if a number is an integer.

Lists
-----

Functions for manipulating lists.

``list``
~~~~~~~~

Turns the arguments into a list.

``nil``
~~~~~~~

The empty list.

``head``
~~~~~~~~

Retrieves the first element of a sequence if it exists. Otherwise throws
an exception.

``tail``
~~~~~~~~

Given a non-empty sequence, returns the sequence of all the elements but
the first. If the sequence is empty, throws an exception.

``empty?``
~~~~~~~~~~

True if 'seq' is empty, false otherwise.

``cons``
~~~~~~~~

Adds an element to the front of a list.

``reverse``
~~~~~~~~~~~

Returns the reversed 'list'.

``length``
~~~~~~~~~~

Returns the length of 'list'.

``concat``
~~~~~~~~~~

Concatenates 'list1' and 'list2'.

``filter``
~~~~~~~~~~

Returns 'list' with only the elements that satisfy 'filter-cond'.

``range``
~~~~~~~~~

Returns a list with all integers from 'start' to 'end', inclusive.

``list-with-head``
~~~~~~~~~~~~~~~~~~

Given a value ``x``, and two functions ``f`` and ``g``, checks if ``x``
is a list with a head. If so applies ``f`` to the head, otherwise calls
``g`` with no args.

``for-each``
~~~~~~~~~~~~

(for-each ls cb) calls ``cb`` for each element in the list ``ls``, for
its side effect. always returns ``nil``.

Vectors
-------

Functions for manipulating vectors.

``<>``
~~~~~~

Concatenates two vectors.

``add-left``
~~~~~~~~~~~~

Adds an element to the left side of a vector.

``add-right``
~~~~~~~~~~~~~

Adds an element to the right side of a vector.

``for-each-vec``
~~~~~~~~~~~~~~~~

(for-each-vec v cb) calls ``cb`` for each element in ``v``, for its side
effect. always returns ``nil``.

Sequences
---------

Functions for manipulating boths lists and vectors.

``nth``
~~~~~~~

Given an integral number ``n`` and ``xs``, returns the ``n``\ th element
(zero indexed) of ``xs`` when ``xs`` is a list or a vector. If ``xs``
does not have an ``n``-th element, or if it is not a list or vector,
then an exception is thrown.

``foldl``
~~~~~~~~~

Given a function ``f``, an initial value ``i`` and a sequence (list or
vector) ``xs``, reduces ``xs`` to a single value by starting with ``i``
and repetitively combining values with ``f``, using elements of ``xs``
from left to right.

``foldr``
~~~~~~~~~

Given a function ``f``, an initial value ``i`` and a sequence (list or
vector) ``xs``, reduces ``xs`` to a single value by starting with ``i``
and repetitively combining values with ``f``, using elements of ``xs``
from right to left.

``map``
~~~~~~~

Given a function ``f`` and a sequence (list or vector) ``xs``, returns a
sequence of the same size and type as ``xs`` but with ``f`` applied to
all the elements.

``seq``
~~~~~~~

Given a structure ``s``, returns a sequence. Lists and vectors are
returned without modification while for dicts a vector of
key-value-pairs is returned: these are vectors of length 2 whose first
item is a key and whose second item is the associated value.

``take``
~~~~~~~~

Returns the first ``n`` items of a sequence, unless the sequence is too
short, in which case an exception is thrown.

``drop``
~~~~~~~~

Returns all but the first ``n`` items of a sequence, unless the sequence
is empty, in which case an exception is thrown.

Dicts
-----

Functions for manipulating dicts.

``dict``
~~~~~~~~

Given an even number of arguments, creates a dict where the ``2i``-th
argument is the key for the ``2i+1``\ th argument. If one of the even
indexed arguments is not hashable then an exception is thrown.

``lookup``
~~~~~~~~~~

Given a value ``k`` (the 'key') and a dict ``d``, returns the value
associated with ``k`` in ``d``. If the key does not exist in ``d`` then
``()`` is returned instead. If ``d`` is not a dict then an exception is
thrown.

``insert``
~~~~~~~~~~

Given ``k``, ``v`` and a dict ``d``, returns a dict with the same
associations as ``d`` but with ``k`` associated to ``d``. If ``d`` isn't
a dict or if ``k`` isn't hashable then an exception is thrown.

``delete``
~~~~~~~~~~

Given ``k`` and a dict ``d``, returns a dict with the same associations
as ``d`` but without the key ``k``. If ``d`` isn't a dict then an
exception is thrown.

``dict-from-list``
~~~~~~~~~~~~~~~~~~

Creates a dictionary from a list of key-value pairs.

``keys``
~~~~~~~~

Given a ``dict``, returns a vector of its keys.

``values``
~~~~~~~~~~

Given a ``dict``, returns a vector of its values.

``rekey``
~~~~~~~~~

Change the key from 'old-key' to 'new-key' in 'dict'. If 'new-key'
already exists, it is overwritten.

``map-values``
~~~~~~~~~~~~~~

Given a function ``f`` and a dict ``d``, returns a dict with the same
keys as ``d`` but ``f`` applied to all the associated values.

``modify-map``
~~~~~~~~~~~~~~

Given a key, a function and a dict, applies the function to the value
associated to that key.

Structures
----------

Functions for manipulating lists, vectors and dicts.

``member?``
~~~~~~~~~~~

Given ``v`` and structure ``s``, checks if ``x`` exists in ``s``. The
structure ``s`` may be a list, vector or dict. If it is a list or a
vector, it checks if ``v`` is one of the items. If ``s`` is a dict, it
checks if ``v`` is one of the keys.

Refs
----

Functions for creating, querying and modifying refs.

``ref``
~~~~~~~

Creates a ref with the argument as the initial value.

``read-ref``
~~~~~~~~~~~~

Returns the current value of a ref.

``write-ref``
~~~~~~~~~~~~~

Given a reference ``r`` and a value ``v``, updates the value stored in
``r`` to be ``v`` and returns ``v``.

``modify-ref``
~~~~~~~~~~~~~~

Modify 'ref' by applying the provided function. Returns the new value.

Evaluation functions
--------------------

Utilities for creating and extending evaluation functions.

``base-eval``
~~~~~~~~~~~~~

The default evaluation function. Expects an expression and a radicle
state. Return a list of length 2 consisting of the result of the
evaluation and the new state.

``eval``
~~~~~~~~

An eval in which one can use ``(:enter-chain url)`` to make the eval
behave as that of a remote chain, and ``:send`` to send all enqueued
expressions.

``updatable-eval``
~~~~~~~~~~~~~~~~~~

Given an evaluation function ``f``, returns a new one which augments
``f`` with a new command ``(update expr)`` which evaluates arbitrary
expression using ``base-eval``.

Documentation and testing
-------------------------

Functions for creating and querying documentation of variables in scope,
and testing functions.

``help``
~~~~~~~~

Default help text.

``doc``
~~~~~~~

Returns the documentation string for a variable. To print it instead,
use ``doc!``.

``doc!``
~~~~~~~~

Prints the documentation attached to a value and returns ``()``. To
retrieve the docstring as a value use ``doc`` instead.

``apropos!``
~~~~~~~~~~~~

Prints documentation for all documented variables in scope.

``document``
~~~~~~~~~~~~

Used to add documentation to variables.

``is-test-env``
~~~~~~~~~~~~~~~

True iff file is being run as part of the Haskell suite

Environment functions
---------------------

Utilities for modifying the current environment.

``pure-env``
~~~~~~~~~~~~

Returns a pure initial radicle state. This is the state of a radicle
chain before it has processed any inputs.

``get-current-env``
~~~~~~~~~~~~~~~~~~~

Returns the current radicle state.

``set-current-env``
~~~~~~~~~~~~~~~~~~~

Replaces the radicle state with the one provided.

``set-env!``
~~~~~~~~~~~~

Given an atom ``x`` and a value ``v``, sets the value associated to
``x`` in the current environemtn to be ``v``. Doesn't evaluate ``v``.

Input/Output
------------

Effectful functions. These functions are not available in 'pure' chains,
but are available in the local REPL.

``put-str!``
~~~~~~~~~~~~

Outputs a string verbatim.

``print!``
~~~~~~~~~~

Pretty-prints a value.

``get-line!``
~~~~~~~~~~~~~

Reads a single line of input and returns it as a string.

``get-expression!``
~~~~~~~~~~~~~~~~~~~

Reads a line of input and returns it as a string. Takes a list of
completion words.

``load!``
~~~~~~~~~

Evaluates the contents of a file. Each seperate radicle expression is
``eval``\ uated according to the current definition of ``eval``.

``read-file!``
~~~~~~~~~~~~~~

Reads the contents of a file and returns it as a string.

``read-code!``
~~~~~~~~~~~~~~

Read code (as data) from a file. Returns a vector of expressions

``send-code!``
~~~~~~~~~~~~~~

Send code from a file to a remote chain.

``send-prelude!``
~~~~~~~~~~~~~~~~~

Send the pure prelude to a chain.

``subscribe-to!``
~~~~~~~~~~~~~~~~~

Expects a dict ``s`` (representing a subscription) and a function ``f``.
The dict ``s`` should have a function ``getter`` at the key ``:getter``.
This function is called repeatedly (with no arguments), its result is
then evaluated and passed to ``f``.

``uuid!``
~~~~~~~~~

Generates a random UUID.

``read-line!``
~~~~~~~~~~~~~~

Read a single line of input and interpret it as radicle data.

``exit!``
~~~~~~~~~

Exit the interpreter immediately.

Lenses
------

Functional references into radicle values.

``@``
~~~~~

Returns a lens targetting keys of dicts.

``@nth``
~~~~~~~~

Lenses into the nth element of a vector

``make-lens``
~~~~~~~~~~~~~

Makes a lens out of a getter and a setter.

``view``
~~~~~~~~

View a value through a lens.

``view-ref``
~~~~~~~~~~~~

Like 'view', but for refs.

``set``
~~~~~~~

Set a value though a lens.

``set-ref``
~~~~~~~~~~~

Like 'set', but for refs.

``over``
~~~~~~~~

Modify a value through a lens.

``over-ref``
~~~~~~~~~~~~

Like 'over', but for refs.

``id-lens``
~~~~~~~~~~~

The identity lens.

``..``
~~~~~~

Compose two lenses.

``...``
~~~~~~~

Compose multiple lenses.

Validation
----------

Functions for creating or combining *validators*, which are functions
which return the input unchanged or throw with an error message. These
can be used for checking data before accepting it onto a chain.

``validator/=``
~~~~~~~~~~~~~~~

Given ``x``, returns a validator that checks for equality with ``x``.

``validator/member``
~~~~~~~~~~~~~~~~~~~~

Given a structure, returns a validator which checks for membership in
the structure.

``validator/type``
~~~~~~~~~~~~~~~~~~

Checks that a value has a type. Expects a keyword describing the type,
as returned by the ``type`` function.

``validator/pred``
~~~~~~~~~~~~~~~~~~

Given a description and a predicate, returns a validator that checks if
the predicate is true.

``validator/every``
~~~~~~~~~~~~~~~~~~~

Given a validator, creates a new validator which checks that all the
items in a sequence conform to it.

``validator/and``
~~~~~~~~~~~~~~~~~

Given a sequence of validators ``vs``, returns a new validator which,
given a value, checks if it conforms to all the validators in ``vs``.

``validator/or``
~~~~~~~~~~~~~~~~

Given a vector of validators ``vs``, returns a new validator which,
given a value, checks if it conforms to at least one of the ``vs``.

``validator/key``
~~~~~~~~~~~~~~~~~

Given a key and a validator, returns a validator which checks for the
existence of that key and that the associated value conforms to the
validator.

``validator/keys``
~~~~~~~~~~~~~~~~~~

Given a dict associating keys to validators, returns a validator which
checks a dict for the existence of those keys, and that they conform to
the associated validators.

Cryptography
------------

Tools for creating and verifying cryptographic signatures, and
generating private/public key pairs.

``verify-signature``
~~~~~~~~~~~~~~~~~~~~

Given a public key ``pk``, a signature ``s`` and a message (string)
``m``, checks that ``s`` is a signature of ``m`` for the public key
``pk``.

``default-ecc-curve``
~~~~~~~~~~~~~~~~~~~~~

Returns the default elliptic-curve used for generating cryptographic
keys.

``gen-key-pair!``
~~~~~~~~~~~~~~~~~

Given an elliptic curve, generates a cryptographic key-pair. Use
``default-ecc-curve`` for a default value for the elliptic curve.

``gen-signature!``
~~~~~~~~~~~~~~~~~~

Given a private key and a message (a string), generates a cryptographic
signature for the message.

Chain tools
-----------

These functions can be used to simulate remote chains in the local REPL.
This is useful for experimenting with inputs or even new evaluation
functions before sending these to a remote chain.

``new-chain``
~~~~~~~~~~~~~

Return an empty chain dictionary with the given url.

``@var``
~~~~~~~~

A lens for variables in states of chains.

``eval-in-chain``
~~~~~~~~~~~~~~~~~

Evaluates 'expr' in the 'chain' and returns a dict with the ':result'
and the resulting ':chain'.

``enter-remote-chain``
~~~~~~~~~~~~~~~~~~~~~~

Make the eval behave as that of a remote chain. The second param is the
env to return to after :quit.

``update-chain``
~~~~~~~~~~~~~~~~

Takes a chain, and returns a new chain updated with the new expressions
from the remote chain

``add-quit``
~~~~~~~~~~~~

Adds a ':quit' command to 'before-quit-eval', which switches to
'after-quit-state' (and to the eval in that state)

``add-send``
~~~~~~~~~~~~

Add a :send special form that sends the contents of *input to the
chain*\ cur-chain

``load-chain``
~~~~~~~~~~~~~~

Takes a ``url``, and fetches the inputs of a remote chain and return a
chain dictionary with the chain state.

``pure-prelude-files``
~~~~~~~~~~~~~~~~~~~~~~

List of files which together define the pure prelude.

``pure-prelude-code``
~~~~~~~~~~~~~~~~~~~~~

The pure prelude.

``store-exprs``
~~~~~~~~~~~~~~~

Store each new evaluated expression in '_inputs'

``eval-fn-app``
~~~~~~~~~~~~~~~

Given a state, a function, an argument and a callback, returns the
result of evaluating the function call on the arg in the given state,
while also calling the callback on the result.

``state-machine-eval``
~~~~~~~~~~~~~~~~~~~~~~

Returns an eval which operates a state machine whose transition function
may be updated. To update the transition function all voters must agree
on it.

``state-machine-input``
~~~~~~~~~~~~~~~~~~~~~~~

Handle an input in the morphing state-machine.

``state-machine-new-trans``
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Trigger a new vote.

``state-machine-agree``
~~~~~~~~~~~~~~~~~~~~~~~

Vote to agree on a new transition function.

``state-machine-disagree``
~~~~~~~~~~~~~~~~~~~~~~~~~~

Vote to disagree on a new transition function.

``simple-trans``
~~~~~~~~~~~~~~~~

Given a function ``f``, makes a transition function who's output is also
the next state.

``update-chain-ref``
~~~~~~~~~~~~~~~~~~~~

Update a ref containing a chain with the new expressions from the remote
chain

Issue chain
-----------

These functions allow creating and interacting with the default issues
chain.

``create-issues-chain!``
~~~~~~~~~~~~~~~~~~~~~~~~

Create a remote issue chain with the given url. Returns a ref with the
chain.

``list-issues``
~~~~~~~~~~~~~~~

Given an issues ref ``x``, returns its issues. Does not itself update
the chain.

``new-issue!``
~~~~~~~~~~~~~~

Create a new remote issue. Takes a chain ref, a keypair, an issue title
and an issue body.