module OuterFirst where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- We only need to use return once because it's a big Monad
embedded :: MaybeT
            (ExceptT String
                     (ReaderT () IO))
            Int
embedded = return 1

-- peel back MaybeT
maybeUnwrap :: ExceptT String
                       (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

-- peel back ExceptT
eitherUnwrap :: ReaderT () IO
                (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

-- peel back ReaderT
readerUnwrap :: ()
             -> IO (Either String
                           (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

-- GHCi> readerUnwrap ()
-- Right (Just 1)
