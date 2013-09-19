{-# LANGUAGE DeriveFunctor #-}
module Conf where

import Pipes (ListT,enumerate)
import Pipes.Prelude (toListM)

import Control.Monad.Trans.Free (FreeT,FreeF(Pure,Free),runFreeT,liftF,wrap)
import Control.Monad.Trans.State (StateT,runStateT,modify)
import Control.Monad.Identity (Identity,runIdentity)
import Control.Monad.Trans (lift)
import Control.Monad (mzero,mplus)

import Data.Set (Set,insert)

type Conf flag = ConfT flag Identity

type ConfT flag = FreeT (ConfF flag)

data ConfF flag a = Require flag a
                  | Fork a a
                  | Failure String
                    deriving (Functor,Show,Read,Eq)

require :: (Monad m) => flag -> ConfT flag m ()
require flag = liftF (Require flag ())

fork :: (Monad m) => ConfT flag m a -> ConfT flag m a -> ConfT flag m a
fork branch1 branch2 = wrap (Fork branch1 branch2)

failure :: (Monad m) => String -> ConfT flag m a
failure message = liftF (Failure message)

runConf :: (Ord flag) => Conf flag a -> Set flag -> [(a,Set flag)]
runConf conf flags = runIdentity (runConfT conf flags)

runConfT :: (Monad m,Ord flag) => ConfT flag m a -> Set flag -> m [(a,Set flag)]
runConfT conft flags = toListM (enumerate (runStateT (interpretConfT conft) flags))

interpretConfT :: (Monad m,Ord flag) => ConfT flag m a -> StateT (Set flag) (ListT m) a
interpretConfT conft = do
    nextstep <- lift (lift (runFreeT conft))
    case nextstep of
        Pure a -> return a
        Free (Require flag continue) -> modify (insert flag) >> interpretConfT continue
        Free (Fork continue1 continue2) -> interpretConfT continue1 `mplus` interpretConfT continue2
        Free (Failure _) -> lift mzero
