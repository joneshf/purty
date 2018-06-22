module App where

import "rio" RIO

import "mtl" Control.Monad.Except
    ( ExceptT
    , MonadError
    , runExceptT
    )
import "transformers" Control.Monad.Trans.Except (catchE)

newtype App env error c
  = App (ReaderT env (ExceptT error IO) c)
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadError error
    , MonadIO
    , MonadReader env
    )

handle :: App a b c -> (b -> App a d c) -> App a d c
handle (App (ReaderT f)) g = App $ ReaderT $ \env ->
  catchE (f env) $ \y -> case g y of
    App h -> runReaderT h env

run :: a -> App a Void b -> IO b
run env (App f) = do
  result <- runExceptT (runReaderT f env)
  either absurd pure result
