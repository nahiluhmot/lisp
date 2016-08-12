{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lisp.Monad ( LispT
                  , runLispT
                  )where

import Control.Monad.Except
import Control.Monad.State.Class

data LispT e s m a = Pure !a
                   | Raise !e
                   | Get !(s -> LispT e s m a)
                   | Put !s !(LispT e s m a)
                   | Lift !(m (LispT e s m a))

runLispT :: Monad m => LispT e s m a -> s -> m (Either e a, s)
runLispT =
  let go (Pure a) s = return (Right a, s)
      go (Raise e) s = return (Left e, s)
      go (Get f) s = go (f s) s
      go (Put s' n) _ = go n s'
      go (Lift m) s = m >>= flip go s
  in  go

instance Functor m => Functor (LispT e s m) where
  fmap f =
    let go (Pure a) = Pure (f a)
        go (Raise e) = Raise e
        go (Get g) = Get (go . g)
        go (Put s n) = Put s (go n)
        go (Lift m) = Lift (fmap go m)
    in  go

instance Functor m => Applicative (LispT e s m) where
  pure = Pure
  (<*>) =
    let go (Pure f) (Pure a) = Pure (f a)
        go (Raise e) _ = Raise e
        go (Get f) n = Get (flip go n . f)
        go (Put s n) n' = Put s (go n n')
        go (Lift m) n = Lift (fmap (flip go n) m)
        go _ (Raise e) = Raise e
        go n (Get f) = Get (go n . f)
        go n (Put s n') = Put s (go n n')
        go n (Lift m) = Lift (fmap (go n) m)
    in  go

instance Functor m => Monad (LispT e s m) where
  return = Pure
  action >>= f =
    let go (Pure a) = f a
        go (Raise e) = Raise e
        go (Get g) = Get (go . g)
        go (Put s n) = Put s (go n)
        go (Lift m) = Lift (fmap go m)
    in  go action

instance MonadTrans (LispT e s) where
  lift = Lift . fmap Pure

instance Functor m => MonadState s (LispT e s m) where
  get = Get Pure
  put s = Put s (Pure ())
  state f =
    Get $ \s ->
      let (a, s') = f s
      in  Put s' (Pure a)

instance Functor m => MonadError e (LispT e s m) where
  throwError = Raise
  catchError action f =
    let go (Pure a) = Pure a
        go (Raise e) = f e
        go (Get g) = Get (go . g)
        go (Put s n) = Put s (go n)
        go (Lift m) = Lift (fmap go m)
    in  go action

instance MonadIO m => MonadIO (LispT e s m) where
  liftIO = Lift . fmap Pure . liftIO
