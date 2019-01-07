Testing
=======

Radicle provides a ``:test`` macro which allows you to write tests next
to your code.

.. code-block:: radicle

  (def not
    (fn [x] (if x #f #t)))

  (:test "not"
     [ (not #t) ==> #f ]
     [ (not #f) ==> #t ]
     )

The ``test/prelude.rad`` script runs all tests defined in the prelude.


Test Definition
---------------

Each test definition consists of a test name and a list of steps

.. code-block:: radicle

  (:test "my test" step1 step2 ...)

Each step is a vector triple with the symbol ``==>`` in the middle. For
example

.. code-block:: radicle

  [ (not #t) ==> #f ]

When a test step is run the value left of ``==>`` is evaluated in the
environment captured at the definition site of the test. The resulting value is
then compared with the right-hand side. The test passes if both are
equal. The right-hand side is *not* evaluated.

If the evaluation of the left-hand side throws an error the test fails
with the message produced by the error.

Changes to reference in a test step evaluation have no effect and all
tests steps are run independently.


Test Setup
----------

There is a special setup test step that allows you to change the
environment that tests steps run in.

.. code-block:: radicle

  (:test "with setup"
    [ :setup (do
        (def foo 5)
        )]
    [ (+ foo 2) ==> 7 ]
    [ (- foo 2) ==> 3 ]
    )

Similar to test steps the body of the ``:setup`` step is evaluated in
the environment of the definition site. Changes to the environment
introduced by evaluating the setup code are then available in all test
steps.


Running Tests
-------------

All tests defined with the ``:test`` macro are collected in the ``tests``
reference. Tests can be executed using the ``run-all`` function from
the ``prelude/test`` module.
