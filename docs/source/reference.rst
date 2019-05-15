Radicle Reference
=================

This is the ``radicle`` reference document, with documentation for all
functions which come as part of the standard distribution.

Primitive functions
-------------------

Primitive functions are those that are built into the compiler. They are
available on all machines but may be shadowed by later definitions.
Those that end in a ``!`` are only available locally, not on 'pure'
machines.

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

``show-unformatted``
~~~~~~~~~~~~~~~~~~~~

Returns a string representing the argument value. (No extra formatting)

``throw``
~~~~~~~~~

Throws an exception. The first argument should be an atom used as a
label for the exception, the second can be any value.

``exit!``
~~~~~~~~~

Exit the interpreter immediately with the given exit code.

``read-annotated``
~~~~~~~~~~~~~~~~~~

``(read-anotated label s)`` parses the string ``s`` into a radicle
value. The resulting value is not evaluated. The ``label`` argument is a
string which is used to annotate the value with line numbers.

``read-many-annotated``
~~~~~~~~~~~~~~~~~~~~~~~

(read-many-annotated label s) parses a string into a vector of radicle
values. The resulting values are not evaluated. The ``label`` argument
is a string which is used to annotate the values with line numbers.

``base-eval``
~~~~~~~~~~~~~

The default evaluation function. Expects an expression and a radicle
state. Return a list of length 2 consisting of the result of the
evaluation and the new state.

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

``cons``
~~~~~~~~

Adds an element to the front of a sequence.

``first``
~~~~~~~~~

Retrieves the first element of a sequence if it exists. Otherwise throws
an exception.

``rest``
~~~~~~~~

Given a non-empty sequence, returns the sequence of all the elements but
the first. If the sequence is empty, throws an exception.

``add-right``
~~~~~~~~~~~~~

Adds an element to the right side of a vector.

``<>``
~~~~~~

Merges two structures together. On vectors and lists this performs
concatenation. On dicts this performs the right-biased merge.

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

``length``
~~~~~~~~~~

Returns the length of a vector, list, or string.

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

DEPRECATED Use ``length`` instead. Returns the length of a string.

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
``:list``, ``:vector``, ``:function``, ``:dict``, ``:ref``.

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

``find-module-file!``
~~~~~~~~~~~~~~~~~~~~~

Find a file according to radicle search path rules. These are: 1) If
RADPATH is set, first search there; 2) If RADPATH is not set, search in
the distribution directory 3) If the file is still not found, search in
the current directory.

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

``pure-state``
~~~~~~~~~~~~~~

Returns a pure initial radicle state. This is the state of a radicle
chain before it has processed any inputs.

``get-current-state``
~~~~~~~~~~~~~~~~~~~~~

Returns the current radicle state.

``set-current-state``
~~~~~~~~~~~~~~~~~~~~~

Replaces the radicle state with the one provided.

``get-binding``
~~~~~~~~~~~~~~~

Lookup a binding in a radicle env.

``set-binding``
~~~~~~~~~~~~~~~

Add a binding to a radicle env.

``set-env``
~~~~~~~~~~~

Sets the environment of a radicle state to a new value. Returns the
updated state.

``state->env``
~~~~~~~~~~~~~~

Extract the environment from a radicle state.

``timestamp?``
~~~~~~~~~~~~~~

Returns true if the input is an ISO 8601 formatted CoordinatedUniversal
Time (UTC) timestamp string. If the input isn't a string, an exception
is thrown.

``unix-epoch``
~~~~~~~~~~~~~~

Given an ISO 8601 formatted Coordinated Universal Time (UTC) timestamp,
returns the corresponding Unix epoch time, i.e., the number of seconds
since Jan 01 1970 (UTC).

``from-unix-epoch``
~~~~~~~~~~~~~~~~~~~

Given an integer the represents seconds from the unix epock return an
ISO 8601 formatted Coordinated Universal Time (UTC) timestamp
representing that time.

``now!``
~~~~~~~~

Returns a timestamp for the current Coordinated Universal Time (UTC),
right now, formatted according to ISO 8601.

``to-json``
~~~~~~~~~~~

Returns a JSON formatted string representing the input value. Numbers
are only converted if they have a finite decimal expansion. Strings and
booleans are converted to their JSON counterparts. Atoms and keywords
are converted to JSON strings (dropping the initial ':' for keywords).
Lists and vectors are converted to JSON arrays. Dicts are converted to
JSON objects as long as all the keys are strings, atoms, keywords,
booleans or numbers.

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

``get-args!``
~~~~~~~~~~~~~

Returns the list of the command-line arguments the script was called
with

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

``cd!``
~~~~~~~

Change the current working directory.

``stdin!``
~~~~~~~~~~

A handle for standard in.

``stdout!``
~~~~~~~~~~~

A handle for standard out.

``stderr!``
~~~~~~~~~~~

A handle for standard error.

``read-file!``
~~~~~~~~~~~~~~

Reads the contents of a file and returns it as a string.

``read-line-handle!``
~~~~~~~~~~~~~~~~~~~~~

Read a single line from a handle. Returns the string read, or the
keyword ``:eof`` if an EOF is encountered.

``open-file!``
~~~~~~~~~~~~~~

Open file in the specified mode (``:read``, ``:write``, ``:append``,
``:read-write``).

``close-handle!``
~~~~~~~~~~~~~~~~~

Close a handle

``system!``
~~~~~~~~~~~

(system! proc) execute a system process. Returns the dict with the form
``{ :stdin maybe-handle      :stdout maybe-handle      :stderr maybe-handle      :proc prochandle    }``
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

``length``
~~~~~~~~~~

Returns the length of a vector, list, or string.

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

``head``
~~~~~~~~

Backwards compatible alias for ``first``.

``tail``
~~~~~~~~

Backwards compatible alias for ``rest``.

``(read s)``
~~~~~~~~~~~~

Reads a radicle value from a string.

``(read-many s)``
~~~~~~~~~~~~~~~~~

Reads many radicle values from a string.

``(<= x y)``
~~~~~~~~~~~~

Test if ``x`` is less than or equal to ``y``.

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

A pattern for sequences with a head and a tail.

``(/nil v)``
~~~~~~~~~~~~

Empty-sequence pattern. Matches ``[]`` and ``(list)``

``(/just pat)``
~~~~~~~~~~~~~~~

Pattern which matches ``[:just x]``.

``(/member vs)``
~~~~~~~~~~~~~~~~

Matches values that are members of a structure.

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

``(and-predicate f g)``
~~~~~~~~~~~~~~~~~~~~~~~

Pointwise conjunction of predicates.

``prelude/seq``
---------------

Functions for manipulating sequences, that is lists and vectors.

``(empty? seq)``
~~~~~~~~~~~~~~~~

True if ``seq`` is empty, false otherwise.

``(seq? x)``
~~~~~~~~~~~~

Returns ``#t`` if ``x`` is a list or a vector.

``(reverse xs)``
~~~~~~~~~~~~~~~~

Returns the reversed sequence ``xs``.

``(filter pred ls)``
~~~~~~~~~~~~~~~~~~~~

Returns ``ls`` with only the elements that satisfy ``pred``.

``(take-while pred ls)``
~~~~~~~~~~~~~~~~~~~~~~~~

Returns all elements of a sequence ``ls`` until one does not satisfy
``pred``

``(starts-with? s prefix)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Returns ``#t`` if ``prefix`` is a prefix of the sequence ``s``. Also
works for strings

``(/prefix prefix rest-pat)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Matches sequences that start with ``prefix`` and bind the rest of that
sequence to ``rest-pat``. Also works for strings.

``(concat ss)``
~~~~~~~~~~~~~~~

Concatenate a sequence of sequences.

``prelude/list``
----------------

Functions for creating lists. See also ``prelude/seq``.

``nil``
~~~~~~~

The empty list.

``(range from to)``
~~~~~~~~~~~~~~~~~~~

Returns a list with all integers from ``from`` to ``to``, inclusive.

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

``(map-string f xs)``
~~~~~~~~~~~~~~~~~~~~~

Returns a string consisting of the results of applying ``f`` to each
character of ``xs``. Throws a type error if ``f`` returns something
other than a string

``(reverse-string str)``
~~~~~~~~~~~~~~~~~~~~~~~~

Reverses ``str``. E.g.: ``(reverse-string "abc")`` == ``"cba"``.

``(ends-with? str substr)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~

True if ``str`` ends with ``substr``

``(pad-right-to l word)``
~~~~~~~~~~~~~~~~~~~~~~~~~

Appends the ``word`` with whitespace to get to length ``l``. If ``word``
is longer than ``l``, the whole word is returned without padding.

``prelude/error-messages``
--------------------------

Functions for user facing error messages. Functions should either have a
descriptive name or additional comment so that the text can be edited
without knowledge of where they are used. To verify changes, tests can
be run with ``stack exec -- radicle test/all.rad``

``(missing-arg arg cmd)``
~~~~~~~~~~~~~~~~~~~~~~~~~

Used for command line parsing when an argument to a command is missing.

``(too-many-args cmd)``
~~~~~~~~~~~~~~~~~~~~~~~

Used for command line parsing when there are too many arguments passed
to a command.

``(missing-arg-for-opt opt valid-args)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Used for command line parsing when an option requires an argument.

``(invalid-arg-for-opt arg opt valid-args)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Used for command line parsing when the argument for an option is
invalid.

``(invalid-opt-for-cmd opt cmd)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Used for command line parsing when the option for a given command is
unkown

``(dir-already-exists dir-name)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``rad project checkout`` is aborted, if there is already a directory
with the name of the project ``dir-name`` in the current directory.

``(git-clone-failure origin name)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``rad project checkout`` is aborted, if cloning the repo ``name`` form
``origin`` failed.

``(upstream-commit-failure)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``rad project init`` is aborted when creating an empty commit failed in
preparation to setting the upstream master branch.

``(upstream-push-failure)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~

``rad project init`` is aborted when pushing the empty commit failed
while setting the upstream master branch.

``(item-not-found item item-number)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Any command on a specific patch/issue aborts if it does not exist.

``(whole-item-number item)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Any command on a specific patch/issue aborts if the provided
``item-number`` is not a whole number.

``(missing-item-number item action)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Any command on a specific patch/issue aborts if the ``item-number`` is
not provided.

``(state-change-failure item state)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

On changing the state of a patch/issue if the daemon returned an error.

``(no-number-returned item)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

On creating a patch/issue, when the creation was successful, but no
patch/issue number was returned.

``(unknown-command cmd)``
~~~~~~~~~~~~~~~~~~~~~~~~~

An unknown command for an app. E.g. ``rad issue foobar``

``(unknown-commit commit)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~

``rad patch propose`` aborts if the provided commit is unknown.

``(parent-commit-not-master commit)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``rad patch propose`` aborts if the provided commit is unknown.

``(checkout-new-branch-failure branch)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``rad patch checkout`` aborts if creating and switching to the patch
branch fails.

``(checkout-master-failure)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``rad patch accept`` aborts if checking out the master branch fails.

``(applying-patch-failure)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``rad patch checkout`` aborts if applying the patch to the patch branch
fails. Conflicts have to be resolved manually.

``(applying-accepted-patch-failure)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``rad patch accept`` aborts if applying the patch to master fails.
Conflicts have to be resolved manually as well as pushing the commit.

``(push-patch-failure)``
~~~~~~~~~~~~~~~~~~~~~~~~

``rad patch accept`` aborts if pushing the patch failed.

``(missing-key-file)``
~~~~~~~~~~~~~~~~~~~~~~

Any request to the machine is aborted, when the key file can't be found.

``(rad-ipfs-name-publish-failure stderr)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Printed when the ``rad ipfs name publish`` command in
``init-git-ipfs-repo`` in ``rad-project`` fails. Takes stderr of the
command as an argument.

``(rad-ipfs-key-gen-failure stderr)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Printed when the ``rad ipfs key gen`` command in ``init-git-ipfs-repo``
in ``rad-project`` fails. Takes stderr of the command as an argument.

``(process-exit-error command args exit-code stderr)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Printed when the a sub process exits with a non-zero exit code. Includes
the stderr output in the message.

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

``(lookup-default key default dict)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Like ``lookup`` but returns ``default`` if the key is not in the map.

``(lookup-maybe key dict)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Like ``lookup`` but returns ``[:just x]`` if the key is not in the map
and ``:nothing`` otherwise.

``(safe-modify-map k f d)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Modifies the association of a value to a key ``k`` in a dict ``d``. The
function ``f`` will receive ``[:just v]`` if ``(eq? (lookup k d) v)``,
otherwise it will receive ``:nothing``. It should return
``[:just new-v]`` to change the value, and ``:nothing`` to remove it.

``(group-by f xs)``
~~~~~~~~~~~~~~~~~~~

Partitions the values of a sequence ``xs`` according to the images under
``f``. The partitions are returned in a dict keyed by the return value
of ``f``.

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
and stderr are inherited. See ``man exec`` for more information on
``execvp``. Returns ``:ok`` if the process exited normally and
``[:error n]`` otherwise. Example: ``(process! "ls" ["-Glah"] "")``.

``(read-line!)``
~~~~~~~~~~~~~~~~

Read a single line of input and interpret it as radicle data.

``(read-file-value! file)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Read a single radicle value from a file.

``(read-file-values! file)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Read many radicle values from a file.

``(shell-with-stdout! command to-write)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Like ``shell!``, but captures the stdout and returns it.

``(shell-no-stdin! command to-write)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Like ``shell!``, but inherits stdin. WARNING: using ``shell!`` with
unsanitized user input is a security hazard! Example:
``(shell-no-stdin! "ls -Glah")``.

``(write-file! filename contents)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Write ``contents`` to file ``filename``.

``(process-with-stdout! command args to-write)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Like ``process!``, but captures stdout.

``(process-with-stdout-stderr-exitcode! command args to-write)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Like ``process-with-stdout!``, but returns a vec
``[stdout stderr exitcode]``. ``exitcode`` is either ``:ok`` or
``[:error n]`` where ``n`` is a number.

``(process-with-stdout-strict! command args to-write)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Like ``process-with-stdout!``, but prints an error message and exits if
the command fails.

``(init-file-dict! file)``
~~~~~~~~~~~~~~~~~~~~~~~~~~

Initiate a file with an empty dict, but only if the file doesn't already
exist.

``(read-file-key! file k)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Read a file key. Assumes that the file contents is a serialised dict.

``(write-file-key! file k v)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Write a key to a file. Assumes that the file contents is a serialised
dict.

``(delete-file-key! file k)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Delete a key from a file. Assumes that the file contents is a serialised
dict.

``(ls!)``
~~~~~~~~~

List the contents of the current working directory

``(modify-file! file f)``
~~~~~~~~~~~~~~~~~~~~~~~~~

Modified the value stored in a file according to the function ``f``.

``(install-fake-filesystem! files)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Installs a fake for ``read-file!`` that simulates the presence of files
in the ``files`` dictionary.

If
``(read-file! path) is called and``\ path\ ``is a key in``\ files\ ``then the value from``\ files\ ``is returned. Otherwise the original``\ read-file!\`
is used.

This requires the ``prelude/test/primitive-stub`` script to be loaded.

``(prompt! prompt)``
~~~~~~~~~~~~~~~~~~~~

Ask for user input with a prompt.

``prelude/exception``
---------------------

Tests for exceptions.

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

``member?``
~~~~~~~~~~~

Query if a value is an element of a set.

``(to-vec s)``
~~~~~~~~~~~~~~

Convert a set to a vector.

``(from-seq xs)``
~~~~~~~~~~~~~~~~~

Create a set from a sequence.

``(key-set d)``
~~~~~~~~~~~~~~~

The set of keys of a dict.

``(subset? xs ys)``
~~~~~~~~~~~~~~~~~~~

Checks if ``xs`` is a subset of ``ys``.

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

``(@def k default)``
~~~~~~~~~~~~~~~~~~~~

Returns a lens targetting keys of dicts with a default value for getting
if the key does not exist in the target.

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

``prelude/io-utils``
--------------------

IO-related utilities

``(fzf-select! xs)``
~~~~~~~~~~~~~~~~~~~~

Select one of many strings with ``fzf``. Requires that ``fzf`` be on the
path. Returns ``[:just x]`` where ``x`` is the selected string, or
``:nothing`` if nothing was selected.

``(edit-in-editor! orig)``
~~~~~~~~~~~~~~~~~~~~~~~~~~

Open ``$EDITOR`` on a file prepopulated with ``orig``. Returns the
contents of the edited file when the editor exits.

``(get-git-config! key)``
~~~~~~~~~~~~~~~~~~~~~~~~~

Get the value associated with a key in git config.

``(set-git-config! key value)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Set the value associated with a key in git config.

``(get-git-commit-data! format commit)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Get data from a ``commit`` via ``show`` specified by ``format``

``(get-git-username!)``
~~~~~~~~~~~~~~~~~~~~~~~

Get the user name stored in git config.

``(process-git-with-exit! args msg)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Processes a git command ``args``. If it fails, the message ``msg`` is
shown and the process exits, otherwise ``:ok`` is passed.

``(base-path!)``
~~~~~~~~~~~~~~~~

Returns the base path for storage of radicle related config files. By
default this is ``$HOME/.config/radicle``. This can be adjusted by
setting ``$XDG_CONFIG_HOME``.

``prelude/key-management``
--------------------------

Providing functions for creating and reading key pairs for signing send
commands. Per default, key pairs are stored in
``$HOME/.config/radicle/my-keys.rad`` this can be adjusted by setting
``$XDG_CONFIG_HOME``.

``(read-keys!)``
~~~~~~~~~~~~~~~~

Reads the keys stored in ``my-keys.rad`` or returns ``:nothing`` if the
file doesn't exist.

``(get-keys!)``
~~~~~~~~~~~~~~~

Like ``read-keys`` but prints an error message and exits the process if
no key file was found.

``(create-keys!)``
~~~~~~~~~~~~~~~~~~

Creates a new key pair and stores it in ``my-keys.rad``. Returns the
full absolute path of the created file.

``(set-fake-keys! keys)``
~~~~~~~~~~~~~~~~~~~~~~~~~

Bypass reading the keys from ``my-keys.rad``, using instead the provided
keys. This is intended for testing.

``(use-fake-keys!)``
~~~~~~~~~~~~~~~~~~~~

Bypass reading the keys from ``my-keys.rad``, using newly-generated
ones. This is intended for testing.

``prelude/machine``
-------------------

Functions for simulating remote machines.

``(eval expr state)``
~~~~~~~~~~~~~~~~~~~~~

Evaluation function that adds :test macro to register tests.

``(updatable-eval sub-eval)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given an evaluation function ``f``, returns a new one which augments
``f`` with a new command ``(update expr)`` which evaluates arbitrary
expression using ``base-eval``.

``(eval-fn-app state f arg cb)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Given a state, a function, an argument and a callback, returns the
result of evaluating the function call on the arg in the given state,
while also calling the callback on the result.

``(send-prelude! machine-id)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Send the pure prelude to a machine.

``(new-machine!)``
~~~~~~~~~~~~~~~~~~

Creates a new machine. Returns the machine name.

``(send-code! machine-id filename)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Send code from a file to a remote machine.

``(send! machine-id inputs)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Update a machine with the vector of ``inputs`` to evaluate. Returns a
vector with the evaluation results.

``(query! machine-id expr)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Send an expression to be evaluated on a machine. Does not alter the
machine.

``(install-remote-machine-fake)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Install test doubles for the ``send!``, ``query!``, and
``new-machine! primitives that use a mutable dictionary to store RSMs. Requires``\ rad/test/stub-primitives\`
to be loaded

``(send-signed-command! machine machine-id cmd payload)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Send a command signed by the keys in ``my-keys.rad``.

``(catch-daemon! f)``
~~~~~~~~~~~~~~~~~~~~~

Catches all ``radicle-daemon`` related errors and just prints them out
to the user.

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

``(integral n)``
~~~~~~~~~~~~~~~~

Validator for whole numbers.

``(optional-key k v)``
~~~~~~~~~~~~~~~~~~~~~~

Given a key ``k`` and a validator ``v``, returns a validator which
checks that the value associated to ``k`` in a dict conforms to ``v``.
If the key is absent, the validator passes.

``(contains k)``
~~~~~~~~~~~~~~~~

Given a value, returns a validator which checks for membership of that
value.

``(contains-all ks)``
~~~~~~~~~~~~~~~~~~~~~

Given a vector of keys, returns a validator which checks that a
structure contains all of them.

``(contains-only ks)``
~~~~~~~~~~~~~~~~~~~~~~

Validator which checks that a dict only contains a subset of a vector of
keys.

``(key k v)``
~~~~~~~~~~~~~

Combines existence and validity of a key in a dict.

``(optional-keys ks)``
~~~~~~~~~~~~~~~~~~~~~~

Given a dict associating keys to validators, returns a validator which
checks that the values associated to those keys in a dict conform to the
corresponding validators.

``(keys d)``
~~~~~~~~~~~~

Given a dict ``d``, returns a validator which checks that a dict
contains all the keys that ``d`` does, and that the associated values a
valid according to the associated validators.

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

``(timestamp x)``
~~~~~~~~~~~~~~~~~

A validator which checks if a string is an ISO 8601 formatted
Coordinated Universal Time (UTC) timestamp.

``(string-of-max-length max-len)``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A validator which checks that it's argument is a string and less than
the specified length.

``(always-valid x)``
~~~~~~~~~~~~~~~~~~~~

A validator that is always valid.

``prelude/util``
----------------

Utility functions. For the moment just a counter.

``(make-counter)``
~~~~~~~~~~~~~~~~~~

Creates a stateful counter. Returns a dict with two keys: the function
at ``:next-will-be`` will return the next number (without incrementing
it), while the function at ``:next`` increments the number and returns
it.