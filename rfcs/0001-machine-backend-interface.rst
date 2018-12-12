Machine Backend Interface Definition
====================================

.. date:: 2018-12-11
.. reviewers::

   @jkarni
   @jameshaydon

Synopsis
--------

We propose an interface for backends that control Radicle State
Machines. This interface replaces the ``StorageBackend`` interface
defined in ``Radicle.Internal.Storage``. It’s design is motivated by
the requirements of implementing the interface for IPFS.

Proposal
--------

The current backend interface is described in
``Radicle.Internal.Storage`` with the following code

.. code-block:: haskell

  data StorageBackend m = StorageBackend
      { storageSend :: Text -> Seq Value -> m (Either Text ())
      -- ^ First argument is the chain ID, second argument is the
      -- values to send.
      , storageReceive :: Text -> Int -> m (Either Text [Value])
      -- ^ First argument is the chain ID, second argument is the
      -- index. Only the expression in the chain with an index greater
      -- then the given index are returned.
      }

To implement the IPFS backend this interface requires an update.

.. code-block:: haskell

  newtype MachineId = MachineId Text

  data MachineBackend index m = MachineBackend
      { machineType :: Text
      , machineUpdate :: MachineId -> [Value] -> m (Either Text index)
      , machineGetLog :: MachineId -> Maybe index -> m (Either Text (index, [Value])
      , machineCreate :: m (Either Text MachineId)
      }

We now describe the changes in detail.

First, we replace the term “chain” with “machine” and update the field
and type names. We introduce a newtype for the machine ID.

The ``machineType`` field is an identifier describing the backend. For
the centralised server that hosts and evaluates machines this will be
``host-eval``, for IPFS this will be ``ipfs``.

We make the index a type parameter. Every entry in the machine log will
have an index. For the centralised server the index becomes ``Natural``
and determines the position in the log. For IPFS the index will be the
content hash of the entry. To use the backend we require ``ToRad`` and
``FromRad`` instances for the index.

For the ``machineGetLog`` method the index parameter is now optional.
``machineGetLog m Nothing`` will get the whole chain.

The behavior of ``machineGetLog`` is changed if an index is provided.
The function will return all the log entries that follow the given
index, excluding the entry with the given index.

``machineGetLog`` will also return the index of the last entry in the
return value.
