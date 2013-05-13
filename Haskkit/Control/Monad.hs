-- | Extensions for Control.Monad libraries.
module Haskkit.Control.Monad (
      lift2, lift3
    , voidM
    ) where

import Control.Monad.Trans (MonadTrans,lift)

-- | Apply 'lift' twice to the given argument.
lift2 :: (MonadTrans t, Monad (t1 m), MonadTrans t1, Monad m) =>
     m a -> t (t1 m) a
lift2 = lift . lift

-- | Apply 'lift' three times to the given argument.
lift3 :: (Monad m, MonadTrans t, MonadTrans t1, MonadTrans t2,
      Monad (t2 m), Monad (t1 (t2 m))) =>
	 m a -> t (t1 (t2 m)) a
lift3 = lift2 . lift

-- | Perform a monadic computation and discard the returned value.
-- 
-- voidM (return 3) == return 3 >> return ()
voidM :: (Monad m) => m a -> m ()
voidM act = act >> return ()
