Radicle Reference
=================

This is the ``radicle`` reference document, with documentation for all
functions which come as part of the standard distribution.

Primitive functions
-------------------

Primitive functions are those that are built into the compiler. They are
available on all chains but may be shadowed by later definitions. Those
that end in a ``!`` are only available locally, not on 'pure' chains.

``*``
~~~~~

Multiplies two numbers together.

``+``
~~~~~

Adds two numbers together.

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

``eq?``
~~~~~~~

Checks if two values are equal.

``apply``
~~~~~~~~~

Calls the first argument (a function) using as arguments the elements of
the the second argument (a list).

``show``
~~~~~~~~

Returns a string representing the argument value.

``throw``
~~~~~~~~~

Throws an exception. The first argument should be an atom used as a
label for the exception, the second can be any value.

``exit!``
~~~~~~~~~

Exit the interpreter immediately.

``read``
~~~~~~~~

Parses a string into a radicle value. Does not evaluate the value.

``read-many``
~~~~~~~~~~~~~

Parses a string into a vector of radicle values. Does not evaluate the
values.

``base-eval``
~~~~~~~~~~~~~

The default evaluation function. Expects an expression and a radicle
state. Return a list of length 2 consisting of the result of the
evaluation and the new state.

``(eval expr env)``
~~~~~~~~~~~~~~~~~~~

Evaluation function that adds :test macro to register tests.

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

``match-pat``
~~~~~~~~~~~~~

The most basic built-in pattern-matching dispatch function.

``head``
~~~~~~~~

Retrieves the first element of a sequence if it exists. Otherwise throws
an exception.

``cons``
~~~~~~~~

Adds an element to the front of a list.

``add-left``
~~~~~~~~~~~~

Adds an element to the left side of a vector.

``add-right``
~~~~~~~~~~~~~

Adds an element to the right side of a vector.

``<>``
~~~~~~

Merges two structures together. On vectors this performs concatenations.
On dicts this performs the right-biased merge.

``list``
~~~~~~~~

Turns the arguments into a list.

``list-to-vec``
~~~~~~~~~~~~~~~

Transforms lists into vectors.

``vec-to-list``
~~~~~~~~~~~~~~~

Transforms vectors to lists.

``zip``
~~~~~~~

Takes two sequences and returns a sequence of corresponding pairs. In
one sequence is shorter than the other, the excess elements of the
longer sequence are discarded.

``map``
~~~~~~~

Given a function ``f`` and a sequence (list or vector) ``xs``, returns a
sequence of the same size and type as ``xs`` but with ``f`` applied to
all the elements.

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

``drop``
~~~~~~~~

Returns all but the first ``n`` items of a sequence, unless the sequence
is empty, in which case an exception is thrown.

``sort-by``
~~~~~~~~~~~

Given a sequence ``xs`` and a function ``f``, returns a sequence with
the same elements ``x`` of ``xs`` but sorted according to ``(f x)``.

``tail``
~~~~~~~~

Given a non-empty sequence, returns the sequence of all the elements but
the first. If the sequence is empty, throws an exception.

``take``
~~~~~~~~

Returns the first ``n`` items of a sequence, unless the sequence is too
short, in which case an exception is thrown.

``nth``
~~~~~~~

Given an integral number ``n`` and ``xs``, returns the ``n``\ th element
(zero indexed) of ``xs`` when ``xs`` is a list or a vector. If ``xs``
does not have an ``n``-th element, or if it is not a list or vector,
then an exception is thrown.

``seq``
~~~~~~~

Given a structure ``s``, returns a sequence. Lists and vectors are
returned without modification while for dicts a vector of
key-value-pairs is returned: these are vectors of length 2 whose first
item is a key and whose second item is the associated value.

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

``member?``
~~~~~~~~~~~

Given ``v`` and structure ``s``, checks if ``x`` exists in ``s``. The
structure ``s`` may be a list, vector or dict. If it is a list or a
vector, it checks if ``v`` is one of the items. If ``s`` is a dict, it
checks if ``v`` is one of the keys.

``map-keys``
~~~~~~~~~~~~

Given a function ``f`` and a dict ``d``, returns a dict with the same
values as ``d`` but ``f`` applied to all the keys. If ``f`` maps two
keys to the same thing, the greatest key and value are kept.

``map-values``
~~~~~~~~~~~~~~

Given a function ``f`` and a dict ``d``, returns a dict with the same
keys as ``d`` but ``f`` applied to all the associated values.

``string-append``
~~~~~~~~~~~~~~~~~

Concatenates a variable number of string arguments. If one of the
arguments isn't a string then an exception is thrown.

``string-length``
~~~~~~~~~~~~~~~~~

Returns the length of a string.

``string-replace``
~~~~~~~~~~~~~~~~~~

Replace all occurrences of the first argument with the second in the
third.

``foldl-string``
~~~~~~~~~~~~~~~~

A left fold on a string. That is, given a function ``f``, an initial
accumulator value ``init``, and a string ``s``, reduce ``s`` by applying
``f`` to the accumulator and the next character in the string
repeatedly.

``type``
~~~~~~~~

Returns a keyword representing the type of the argument; one of:
``:atom``, ``:keyword``, ``:string``, ``:number``, ``:boolean``,
``:list``, ``:vector``, ``:function``, ``:dict``, ``:ref``,
``:function``.

``atom?``
~~~~~~~~~

Checks if the argument is a atom.

``keyword?``
~~~~~~~~~~~~

Checks if the argument is a keyword.

``boolean?``
~~~~~~~~~~~~

Checks if the argument is a boolean.

``string?``
~~~~~~~~~~~

Checks if the argument is a string.

``number?``
~~~~~~~~~~~

Checks if the argument is a number.

``integral?``
~~~~~~~~~~~~~

Checks if a number is an integer.

``vector?``
~~~~~~~~~~~

Checks if the argument is a vector.

``list?``
~~~~~~~~~

Checks if the argument is a list.

``dict?``
~~~~~~~~~

Checks if the argument is a dict.

``file-module!``
~~~~~~~~~~~~~~~~

Given a file whose code starts with module metadata, creates the module.
That is, the file is evaluated as if the code was wrapped in
``(module ...)``.

``import``
~~~~~~~~~~

Import a module, making all the definitions of that module available in
the current scope. The first argument must be a module to import. Two
optional arguments affect how and which symbols are imported.
``(import m :as 'foo)`` will import all the symbols of ``m`` with the
prefix ``foo/``. ``(import m '[f g])`` will only import ``f`` and ``g``
from ``m``. ``(import m '[f g] :as 'foo')`` will import ``f`` and ``g``
from ``m`` as ``foo/f`` and ``foo/g``. To import definitions with no
qualification at all, use ``(import m :unqualified)``.

``get-current-env``
~~~~~~~~~~~~~~~~~~~

Returns the current radicle state.

``pure-env``
~~~~~~~~~~~~

Returns a pure initial radicle state. This is the state of a radicle
chain before it has processed any inputs.

``set-current-env``
~~~~~~~~~~~~~~~~~~~

Replaces the radicle state with the one provided.

``set-env!``
~~~~~~~~~~~~

Given an atom ``x`` and a value ``v``, sets the value associated to
``x`` in the current environment to be ``v``. Doesn't evaluate ``v``.

``to-json``
~~~~~~~~~~~

Returns a JSON formatted string representing the input value.

``uuid!``
~~~~~~~~~

Generates a random UUID.

``uuid?``
~~~~~~~~~

Checks if a string has the format of a UUID.

``default-ecc-curve``
~~~~~~~~~~~~~~~~~~~~~

Returns the default elliptic-curve used for generating cryptographic
keys.

``verify-signature``
~~~~~~~~~~~~~~~~~~~~

Given a public key ``pk``, a signature ``s`` and a message (string)
``m``, checks that ``s`` is a signature of ``m`` for the public key
``pk``.

``public-key?``
~~~~~~~~~~~~~~~

Checks if a value represents a valid public key.

``gen-key-pair!``
~~~~~~~~~~~~~~~~~

Given an elliptic curve, generates a cryptographic key-pair. Use
``default-ecc-curve`` for a default value for the elliptic curve.

``gen-signature!``
~~~~~~~~~~~~~~~~~~

Given a private key and a message (a string), generates a cryptographic
signature for the message.

``put-str!``
~~~~~~~~~~~~

Prints a string.

``get-line!``
~~~~~~~~~~~~~

Reads a single line of input and returns it as a string.

``load!``
~~~~~~~~~

Evaluates the contents of a file. Each seperate radicle expression is
``eval``\ uated according to the current definition of ``eval``.

``read-file!``
~~~~~~~~~~~~~~

Reads the contents of a file and returns it as a string.

``read-line-handle!``
~~~~~~~~~~~~~~~~~~~~~

Read a single line from a handle.

``now!``
~~~~~~~~

Returns a timestamp for the current Coordinated Universal Time (UTC),
right now, formatted according to ISO 8601.

``system!``
~~~~~~~~~~~

(system! proc) execute a system process. Returns the dict with the form
``{ :stdin maybe-handle :stdout maybe-handle :stderr maybe-handle :proc prochandle }``
Where ``maybe-handle`` is either ``[:just handle]`` or ``:nothing``.
Note that this is quite a low-level function; higher-level ones are more
convenient.

``wait-for-process!``
~~~~~~~~~~~~~~~~~~~~~

Block until process terminates.

``write-handle!``
~~~~~~~~~~~~~~~~~

Write a string to the provided handle.

``subscribe-to!``
~~~~~~~~~~~~~~~~~

Expects a dict ``s`` (representing a subscription) and a function ``f``.
The dict ``s`` should have a function ``getter`` at the key ``:getter``.
This function is called repeatedly (with no arguments), its result is
then evaluated and passed to ``f``.

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

Prelude modules
---------------

These are the modules included in the radicle prelude and the functions
these modules expose.

``prelude/basic``
-----------------

Basic function used for checking equality, determining the type of a
value, etc.

``(or x y)``
~~~~~~~~~~~~

Returns ``x`` if ``x`` is not ``#f``, otherwise returns ``y``

``(some xs)``
~~~~~~~~~~~~~

Checks that there is a least one truthy value in a list.

``(empty-seq? xs)``
~~~~~~~~~~~~~~~~~~~

Returns true if ``xs`` is an empty sequence (either list or vector).

``(length xs)``
~~~~~~~~~~~~~~~

Returns the length of ``xs``.

``(maybe->>= v f)``
~~~~~~~~~~~~~~~~~~~

Monadic bind for the maybe monad.

``(maybe-foldlM f i xs)``
~~~~~~~~~~~~~~~~~~~~~~~~~

Monadic fold over the elements of a sequence ``xs``, associating to the
left (i.e. from left to right) in the maybe monad.

``(elem? x xs)``
~~~~~~~~~~~~~~~~

Returns true if ``x`` is an element of the sequence ``xs``

``prelude/patterns``
--------------------

Pattern matching is first-class in radicle so new patterns can easily be
defined. These are the most essential.

``(match-pat pat v)``
~~~~~~~~~~~~~~~~~~~~~

The pattern matching dispatch function. This function defines how
patterns are treated in ``match`` expressions. Atoms are treated as
bindings. Numbers, keywords and strings are constant patterns. Dicts of
patterns match dicts whose values at those keys match those patterns.
Vectors of patterns match vectors of the same length, pairing the
patterns and elements by index.

``(_ v)``
~~~~~~~~~

The wildcard pattern.

``(/? p)``
~~~~~~~~~~

Predicate pattern. Takes a predicate function as argument. Values match
against this pattern if the predicate returns a truthy value.

``(/as var pat)``
~~~~~~~~~~~~~~~~~

As pattern. Takes a variable and a sub-pattern. If the subpattern
matches then the whole pattern matches and furthermore the variable is
bound to the matched value.

``(/cons x-pat xs-pat)``
~~~~~~~~~~~~~~~~~~~~~~~~

A pattern for lists with a head and a tail.

``(/nil v)``
~~~~~~~~~~~~

Empty-list pattern.

``(/just pat)``
~~~~~~~~~~~~~~~

Pattern which matches ``[:just x]``.

``prelude/strings``
-------------------

String manipulation functions.

``(intercalate sep strs)``
~~~~~~~~~~~~~~~~~~~~~~~~~~

Intercalates a string in a list of strings

``(unlines x)``
~~~~~~~~~~~~~~~

Concatenate a list of strings, with newlines in between.

``(unwords x)``
~~~~~~~~~~~~~~~

Concatenate a list of strings, with spaces in between.

``(split-by splitter? xs)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Splits a string ``xs`` into a list of strings whenever the function
``splitter?`` returns true for a character.

``(words xs)``
~~~~~~~~~~~~~~

Splits a string ``xs`` into a list of strings by whitespace characters.

``(lines xs)``
~~~~~~~~~~~~~~

Splits a string ``xs`` into a list of strings by linebreaks.

``prelude/io``
--------------

Some basic I/O functions.

``(print! x)``
~~~~~~~~~~~~~~

Print a value to the console or stdout.

``(shell! command to-write)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Executes ``command`` using the shell with ``to-write`` as input. Stdout
and stderr are inherited. WARNING: using ``shell!`` with unsanitized
user input is a security hazard! Example: ``(shell! "ls -Glah" "")``.

``(process! command args to-write)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Executes ``command`` using ``execvp`` with ``to-write`` as input. Stdout
and stderr are inherit. See ``man exec`` for more information on
``execvp``. Example: ``(process! "ls" ["-Glah"] "")``.

``(read-line!)``
~~~~~~~~~~~~~~~~

Read a single line of input and interpret it as radicle data.

``(send-code! chain-id filename)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Send code from a file to a remote chain.

``(read-code! filename)``
~~~~~~~~~~~~~~~~~~~~~~~~~

Read code (as data) from a file. Returns a vector of expressions

``prelude/bool``
----------------

Functions for dealing with truthiness and #f.

``(not x)``
~~~~~~~~~~~

True if ``x`` is ``#f``, false otherwise.

``(and x y)``
~~~~~~~~~~~~~

Returns ``y`` if ``x`` is not ``#f``, otherwise returns ``x``

``(all xs)``
~~~~~~~~~~~~

Checks that all the items of a list are truthy.

``prelude/exception``
---------------------

Tests for exceptions.

``prelude/list``
----------------

Functions for manipulating lists.

``nil``
~~~~~~~

The empty list.

``(empty? seq)``
~~~~~~~~~~~~~~~~

True if ``seq`` is empty, false otherwise.

``(reverse ls)``
~~~~~~~~~~~~~~~~

Returns the reversed list ``ls``.

``(range from to)``
~~~~~~~~~~~~~~~~~~~

Returns a list with all integers from ``from`` to ``to``, inclusive.

``(concat list1 list2)``
~~~~~~~~~~~~~~~~~~~~~~~~

Concatenates ``list1`` and ``list2``.

``(filter pred ls)``
~~~~~~~~~~~~~~~~~~~~

Returns ``ls`` with only the elements that satisfy ``pred``.

``prelude/dict``
----------------

Functions for manipualting dicts.

``(dict-from-seq xs)``
~~~~~~~~~~~~~~~~~~~~~~

Creates a dictionary from a list of key-value pairs.

``(keys d)``
~~~~~~~~~~~~

Given a dict ``d``, returns a vector of its keys.

``(values d)``
~~~~~~~~~~~~~~

Given a dict ``d``, returns a vector of its values.

``(rekey old-key new-key d)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Change the key from ``old-key`` to ``new-key`` in a dict ``d``. If
``new-key`` already exists, it is overwritten.

``(modify-map k f d)``
~~~~~~~~~~~~~~~~~~~~~~

Given a key ``k``, a function ``f`` and a dict ``d``, applies the
function to the value associated to that key.

``(delete-many ks d)``
~~~~~~~~~~~~~~~~~~~~~~

Delete several keys ``ks`` from a dict ``d``.

``prelude/set``
---------------

Sets, built using dicts.

``empty``
~~~~~~~~~

An empty set.

``(insert x s)``
~~~~~~~~~~~~~~~~

Insert a value into a set.

``(delete x s)``
~~~~~~~~~~~~~~~~

Delete a value from a set.

``(member? x s)``
~~~~~~~~~~~~~~~~~

Query if an value is an element of a set.

``(to-vec s)``
~~~~~~~~~~~~~~

Convert a set to a vector.

``(from-seq xs)``
~~~~~~~~~~~~~~~~~

Create a set from a sequence.

``prelude/ref``
---------------

Functions for dealing with reference cells.

``(modify-ref r f)``
~~~~~~~~~~~~~~~~~~~~

Modify ``r`` by applying the function ``f``. Returns the new value.

``prelude/lens``
----------------

Functional references.

``(make-lens g s)``
~~~~~~~~~~~~~~~~~~~

Makes a lens out of a getter and a setter.

``(view lens target)``
~~~~~~~~~~~~~~~~~~~~~~

View a value through a lens.

``(set lens new-view target)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Set a value though a lens.

``id-lens``
~~~~~~~~~~~

The identity lens.

``(.. lens1 lens2)``
~~~~~~~~~~~~~~~~~~~~

Compose two lenses.

``(... lenses)``
~~~~~~~~~~~~~~~~

Compose multiple lenses.

``(over lens f target)``
~~~~~~~~~~~~~~~~~~~~~~~~

Modify a value through a lens.

``(@ k)``
~~~~~~~~~

Returns a lens targetting keys of dicts.

``(@nth n)``
~~~~~~~~~~~~

Lenses into the nth element of a vector

``(view-ref r lens)``
~~~~~~~~~~~~~~~~~~~~~

Like ``view``, but for refs.

``(set-ref r lens v)``
~~~~~~~~~~~~~~~~~~~~~~

Like ``set``, but for refs.

``(over-ref r lens f)``
~~~~~~~~~~~~~~~~~~~~~~~

Like ``over``, but for refs.

``prelude/chain``
-----------------

Functions for simulating remote chains.

``(new-chain url)``
~~~~~~~~~~~~~~~~~~~

Return an empty chain dictionary with the given url.

``(load-chain! url)``
~~~~~~~~~~~~~~~~~~~~~

Takes a ``url``, and fetches the inputs of a remote chain and return a
chain dictionary with the chain state.

``(eval-in-chain expr chain)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Evaluates ``expr`` in the ``chain`` and returns a dict with the
``:result`` and the resulting ``:chain``.

``(update-chain-ref! chain-ref)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Update ``chain-ref`` containing a chain with the new expressions from
the remote chain

``(eval expr env)``
~~~~~~~~~~~~~~~~~~~

An eval in which one can use ``(:enter-chain url)`` to make the eval
behave as that of a remote chain, and ``:send`` to send all enqueued
expressions.

``(updatable-eval sub-eval)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given an evaluation function ``f``, returns a new one which augments
``f`` with a new command ``(update expr)`` which evaluates arbitrary
expression using ``base-eval``.

``(update-chain! chain)``
~~~~~~~~~~~~~~~~~~~~~~~~~

Takes a chain, and returns a new chain updated with the new expressions
from the remote chain

``(eval-fn-app state f arg cb)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given a state, a function, an argument and a callback, returns the
result of evaluating the function call on the arg in the given state,
while also calling the callback on the result.

``(send-prelude! chain-id)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Send the pure prelude to a chain.

``prelude/state-machine``
-------------------------

An eval for running a state-machine with an updatable transition
function.

``prelude/validation``
----------------------

Functions for creating or combining *validators*, which are functions
which return the input unchanged or throw with an error message. These
can be used for checking data before accepting it onto a chain.

``(= x)``
~~~~~~~~~

Given ``x``, returns a validator that checks for equality with ``x``.

``(member xs)``
~~~~~~~~~~~~~~~

Given a structure, returns a validator which checks for membership in
the structure.

``(and vs)``
~~~~~~~~~~~~

Given a sequence of validators ``vs``, returns a new validator which,
given a value, checks if it conforms to all the validators in ``vs``.

``(or vs)``
~~~~~~~~~~~

Given a vector of validators ``vs``, returns a new validator which,
given a value, checks if it conforms to at least one of the ``vs``.

``(type t)``
~~~~~~~~~~~~

Checks that a value has a type. Expects a keyword describing the type,
as returned by the ``type`` function.

``(pred name p)``
~~~~~~~~~~~~~~~~~

Given a description and a predicate, returns a validator that checks if
the predicate is true.

``(key k v)``
~~~~~~~~~~~~~

Given a key and a validator, returns a validator which checks for the
existence of that key and that the associated value conforms to the
validator.

``(keys ks)``
~~~~~~~~~~~~~

Given a dict associating keys to validators, returns a validator which
checks a dict for the existence of those keys, and that they conform to
the associated validators.

``(every v)``
~~~~~~~~~~~~~

Given a validator, creates a new validator which checks that all the
items in a sequence conform to it.

``(uuid x)``
~~~~~~~~~~~~

Validates UUIDs.

``(signed x)``
~~~~~~~~~~~~~~

Checks that a value is a dict with ``:signature`` and ``:author`` keys,
and that the signature is valid for the rest of the dict for that
author. The rest of the dict is turned into a string according to
``show``.

``prelude/util``
----------------

Utility functions. For the moment just a counter.

``(make-counter)``
~~~~~~~~~~~~~~~~~~

Creates a stateful counter. Returns a dict with two keys: the function
at ``:next-will-be`` will return the next number (without incrementing
it), while the function at ``:next`` increments the number and returns
it.