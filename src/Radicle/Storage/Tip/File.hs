module Radicle.Storage.Tip.File
    ( ReadError (..)
    , fsTipStore
    )
where

import           Protolude

import           Control.Monad.Except (liftEither)
import           Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.List (isSuffixOf)
import           System.Directory
                 ( canonicalizePath
                 , doesFileExist
                 , getTemporaryDirectory
                 , listDirectory
                 )
import           System.FilePath ((</>))
import           System.IO (hClose, openBinaryTempFile)
import           System.Posix
                 ( FdOption(SynchronousWrites)
                 , fdToHandle
                 , handleToFd
                 , rename
                 , setFdOption
                 )

import           Radicle.Storage.Tip.Types
import           Radicle.Storage.Types
                 (MachineId, MachineIndex, ReaderOrWriter, toFilePathComponent)


data ReadError mid
    = DecodeError FilePath Text
    -- ^ Contents of tip file 'FilePath' could not be decoded
    | DoesNotExistError FilePath
    -- ^ Tip file at 'FilePath' doesn't exist
    | MachineMismatch mid mid
    -- ^ Expected @mid@, but found @mid@ in tip file
    deriving Show

instance (Typeable mid, Show mid) => Exception (ReadError mid)

fsTipStore
    :: (MachineId mid, MachineIndex tip)
    => FilePath
    -> TipStore (ReadError mid) mid tip IO
fsTipStore dir = TipStore
    { saveTip  = fsSaveTip dir
    , readTip  = fsReadTip dir
    , listTips = fsListTips dir
    }

fsSaveTip
    :: (MachineId mid, MachineIndex tip)
    => FilePath
    -> mid
    -> tip
    -> ReaderOrWriter
    -> IO ()
fsSaveTip dir mid tip mode = do
    let
        prefix  = filePrefix dir mid
        tipFile = prefix <> ".tip"
        tipInfo = TipInfo
            { tipMachineId   = mid
            , tipPointer     = Just tip
            , tipMachineMode = mode
            }
     in do
        tmpFile <-
            bracket hOpen (hClose . snd) $ \(fp, hdl) ->
                fp <$ LBS.hPut hdl (Aeson.encode tipInfo)
        rename tmpFile tipFile
  where
    hOpen = do
        tmpdir    <- getTemporaryDirectory >>= canonicalizePath
        (fp, hdl) <-
            bracketOnError
                (openBinaryTempFile tmpdir "radtip.XXXXX")
                (hClose . snd)
                pure

        -- make extra sure the file is @fsync@ed
        fd <- handleToFd hdl
        setFdOption fd SynchronousWrites True

        (fp,) <$> fdToHandle fd

fsReadTip
    :: (MachineId mid, MachineIndex tip)
    => FilePath
    -> mid
    -> IO (Either (ReadError mid) (TipInfo mid tip))
fsReadTip dir mid = runExceptT $ do
    info <- ExceptT $ readTipFile $ filePrefix dir mid <> ".tip"
    liftEither $ validate info
  where
    validate info =
        let mid' = tipMachineId info
         in if | mid' /= mid -> Left $ MachineMismatch mid mid'
               | otherwise   -> Right info

fsListTips
    :: (MachineId mid, MachineIndex tip)
    => FilePath
    -> IO [Either (ReadError mid) (TipInfo mid tip)]
fsListTips dir = do
    tipFiles <- filter isTipFile <$> listDirectory dir
    traverse readTipFile tipFiles
  where
    isTipFile fp = ".tip" `isSuffixOf` fp

--------------------------------------------------------------------------------

readTipFile
    :: (FromJSON mid, FromJSON tip)
    => FilePath
    -> IO (Either (ReadError mid) (TipInfo mid tip))
readTipFile fp = do
    exists <- doesFileExist fp
    runExceptT $ do
        unless exists $
            throwE $ DoesNotExistError fp

        withExceptT (DecodeError fp . toS) . ExceptT $
            Aeson.eitherDecodeFileStrict' fp

filePrefix :: MachineId mid => FilePath -> mid -> FilePath
filePrefix dir mid = dir </> toFilePathComponent mid
