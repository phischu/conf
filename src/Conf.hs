{-# LANGUAGE DeriveFunctor #-}
module Conf where

import Pipes (ListT,enumerate)
import Pipes.Prelude (toListM)

import Control.Monad.Trans.Free (FreeT,FreeF(Pure,Free),runFreeT,liftF,wrap)
import Control.Monad.Trans.State (StateT,runStateT,modify)
import Control.Monad.Trans.Writer (WriterT,runWriterT,tell)
import Control.Monad.Identity (Identity,runIdentity)
import Control.Monad.Trans (lift)
import Data.Monoid (Monoid)
import Control.Monad (mzero,mplus)

import Data.Set (Set,insert)

type Conf err flag = ConfT err flag Identity

type ConfT err flag = FreeT (ConfF err flag)

data ConfF err flag a = Require flag a
                  | Fork a a
                  | Failure err
                    deriving (Functor,Show,Read,Eq)

require :: (Monad m) => flag -> ConfT err flag m ()
require flag = liftF (Require flag ())

fork :: (Monad m) => ConfT err flag m a -> ConfT err flag m a -> ConfT err flag m a
fork branch1 branch2 = wrap (Fork branch1 branch2)

failure :: (Monad m) => err -> ConfT err flag m a
failure err = liftF (Failure err)

runConf :: (Ord flag,Monoid err) => Conf err flag a -> Set flag -> ([(a,Set flag)],err)
runConf conf flags = runIdentity (runConfT conf flags)

runConfT :: (Monad m,Ord flag,Monoid err) => ConfT err flag m a -> Set flag -> m ([(a,Set flag)],err)
runConfT conft flags = runWriterT (toListM (enumerate (runStateT (interpretConfT conft) flags)))

interpretConfT :: (Monad m,Ord flag,Monoid err) => ConfT err flag m a -> StateT (Set flag) (ListT (WriterT err m)) a
interpretConfT conft = do
    nextstep <- lift (lift (lift (runFreeT conft)))
    case nextstep of
        Pure a -> return a
        Free (Require flag continue) -> modify (insert flag) >> interpretConfT continue
        Free (Fork continue1 continue2) -> interpretConfT continue1 `mplus` interpretConfT continue2
        Free (Failure err) -> lift (lift (tell err)) >> mzero
