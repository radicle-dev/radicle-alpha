# Radicle lang simplifications

## Validation rather than eval redefinition

Eval redefinition is being used to restrict the allowed inputs of a chain. But
in fact most of the logic for rejecting inputs is done by validators, at the
moment placed at the start of each "command" function.

Futhermore once an eval-redefinition has taken place, this heavily restricts the
programming available in the machine (that's the point), but this is annoying
for experimentation at the repl, crafting custom queries, etc. For example we
have the idea that users who wanted JSON responses could just query `(to-json
(list-patches))`, but this is not valid after eval-redefinition.

Here we propose that instead there is a special purpose ref which holds the
machines **root-validator*; a validation function that is used for all inputs to
the machine. Furthermore, the Radicle executable can be configured to bypass the
root-validator ref when one wants to submit experimental inputs. Daemons which
are owner-mode for a machine will obviously never bypass this ref when accepting
new inputs, but the `query` endpoint on local daemon might do this by default.

Futhermore we propose to unify pattern matching and validation. So a validator
function takes in an input `x` and will decide if `x` is valid or not. If it is
not it will produce an explanation (a string) of why `x` is not valid (either by
throwing or returning a `[:invalid _]`, TBD). In case that `x` is valid, it will
return a bindings dict (from atoms to values); values it has
extracted/transformed from `x`. The root-validator is expected to return a
single binding from `input` to the input to run on the machine.

Since validators can futhermore be stateful, this allows the creation of
interesting validation middleware, for example validators which help with
conducting votes, authenticating users, moderating comments, etc.

The counter machine would just be:

```clojure
(def i (ref 0))

(def inc
  (fn [] (modify-ref i (fn [x] (+ 1 x)))))
```

Initially the root-validator is just lets everything through:

``` clojure
(fn [i] {'input i})
```

If we want to lock down the above machine to only accept the `(inc)` command,
then we would add the input:

```clojure
(import prelude/validation :as 'v)

(write-ref _input-validator_ (v/= '(inc)))
```

## Stateful validation middleware

If the participants of a machine decide all inputs should be signed by some
public key, then they could setup some middleware as follows. A higher order
validator would check for inputs that look like:

``` clojure
{:type :signed-input
 :nonce "123"
 :machine-id "abc"
 :input i
 :public-key pk
 :signature s
}
```

It would check that the signature `s` is indeed a signature for the rest of the
dict, and then pass

``` clojure
{:input i
 :author pk
 }
```

to the sub-validator.

Adding state to validators allows one to build validators which manage voting on
inputs. One can imagine a validator which looks for *input proposals*, then
starting a vote on that proposal. When a command from a certain public key to
vote for the proposal comes through, if the proposal doesn't have enough votes
yet rather than pass anything down to a sub-validator, it accepts the input
immediately but transforming it to

```
[:vote-accepted
  {:proposal 23
   :voter-id pk
   :vote :accepted
   }]
```

## Hyperstatic makes reprogramming difficult

We have created a few remote machines and they all seem to have the following:

- A big chunk of "domain" state (e.g. a dict of issues).
- Smaller "metadata" state (e.g. used up nonces, next ID, admin set, etc.).
- A set of commands, doing a lot of validation and a little bit of mutation.

Some ways people might "reprogram" a machine:

1. Fix some of the data (add a default field).
2. Tweak validation of an existing command (add more conditions). Maybe also
   tweak the mutation this command does.
3. Add a whole new command.

Scenario (1) is probably quite common as part of an update which does (2) or
(3), because new features of changes to old features might demand a modified
data format. In any case it is probably quite easy, just a `(modify-ref ...)`.

The first step of scenario (3) is quite easy: add a new function for the new
command. However you must then mutate the input-validator to accept this new
invocation. This might be tricky: the pertinent validator might be under a few
layers of higher order validators. If you are lucky the validtor you want to
mutate was put in a ref (to be dereferenced by other validators) and you can
mutate that directly.

In any case it seems that scenario (2) (tweaking existing functionality) is the
tricky one. Relaxing the hyperstatic environment seems like one way of making
this easier, since now you can just resubmit definitions for the functions you
want to change.

Non-hyperstatic would also allow for old definitions to get garbage collected,
whereas before this wasn't possible because an old definition could still be in
use in some deeply nested closure. Machine participants would then be much more
encouraged to update the modules in use on a machine.
