Data Validation
==================

Most of the entities that get manipulated and stored on machines are represented
by structured radicle data. To maintain the integrety of the machine it can be
important to refuse malformed/invalid data. To detect this, use *validator
functions*. These are functions which take a piece of data as input and either
throw an exception (thus cancelling the current transaction), or return the data
unchanged. The radicle prelude contains functions for creating and combining
validators.

For example let's assume that we want to validate the ``:priority`` field on an
issue entity, with the requirement that it be an integer :math:`p` such that
:math:`0 \leq p \leq 10`. To do this we can define a validator:

.. code-block:: radicle

   (def validator/priority
     (validator/and
       [(validator/type :number)
        (validator/pred "is integer"
                        integral?)
        (validator/pred "0 <= p <= 10"
                        (fn [p] (and (< -1 p) (< p 11))))]))

Here we have used ``validator/and`` to combine several validators together: the
priority is valid only if it passes all three validators:

- The first one checks that the value is a number.

- The second uses ``validator/pred``, which takes a description and a predicate,
and checks that the predicate holds. In this case we ensure that the priority is
a whole number.

- The third validator ensures the priority is in the required range.

Very often entities are represented using dicts with specific required keys.
Validators for these entities can be created using the ``validator/keys``
function. For example to validate an issue we could use:

.. code-block:: radicle

    (def validator/issue
      (validator/keys
        {:id     (validator/and
                   [(validator/type :string)
                    (validator/pred "valid UUID"
                                     uuid?)])
         :author (validator/pred "valid public key"
                                 public-key?)
         :title  (validator/and
                   [(validator/type :string)
                    (validator/pred "< 60 chars"
                                    (fn [s] (< (string-length s) 60)))])
         :body   (validator/and
                   [(validator/type :string)
                    (validator/pred "valid markdown"
                                    markdown?)
                    (validator/pred "< 4000 chars"
                                    (fn [s] (< (string-length s) 4000)))])
         :tags   (validator/every
                   (validator/and
                     [(validator/type :string)
                      (validator/pred "< 40 chars"
                                      (fn [s] (< (string-length s) 40)))]))
         :priority validator/priority}))

This checks that an issue is a dict, with required keys:

- ``:id`` that's a valid UUID string,

- ``:author`` that's a valid public key,

- ``:title`` that's a string shorter than 60 characters,

- ``:body`` that's a markdown string less that 4000 characters,

- ``:tag`` that's a vector of strings, each shorted than 40 characters.

- ``:priority`` that's a integers :math:`p` such that :math:`0 \leq p \leq 10`.

Because validators return their input if it is valid, we can just wrap any value
which should be validated with the appropriate validator:

.. code-block:: radicle

   (fn [issue]
     (modify-ref
       issues
       (fn [is]
         (add-right (validator/issue issue) is))))
