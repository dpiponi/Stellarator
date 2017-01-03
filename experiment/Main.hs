{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Array.IO
import Data.Array.Base
import Control.Lens
import Control.Monad.Reader

newtype TypedIndex t = TO { unTyped :: Int } deriving (Ord, Ix, Eq, Num)

class Monad m => Reg t m where
    load :: TypedIndex t -> m t
    previous :: TypedIndex t -> m t
    store :: TypedIndex t -> t -> m ()
    modify :: TypedIndex t -> (t -> t) -> m ()
    modify r f = do { value <- previous r; store r (f value) }

(@=) :: Reg t m => TypedIndex t -> t -> m ()
(@=) = store

(@->) :: Reg t m => TypedIndex t -> TypedIndex t -> m ()
(@->) src dst = load src >>= (dst @=)

infixr 2 @= 
infix 2 @->

type StoredBit = TypedIndex Bool 

type Segment a = IOUArray (TypedIndex a) a

data Circuit = Circuit {
    _old :: Segment Bool,
    _current :: Segment Bool
}

$(makeLenses ''Circuit)

newtype MonadCircuit a = M { unM :: ReaderT Circuit IO a }
      deriving (Functor, Applicative, Monad, MonadReader Circuit, MonadIO)

instance Reg Bool MonadCircuit where
    {-# INLINE load #-}
    load r = do
        value <- view current
        liftIO $ unsafeRead value (unTyped r)

    previous r = do
        value <- view old
        liftIO $ unsafeRead value (unTyped r)

    {-# INLINE store #-}
    store r v = do
        value <- view current
        liftIO $ unsafeWrite value (unTyped r) v

data HCycle = H01 | H02 deriving Show

cell :: HCycle ->
        StoredBit -> StoredBit -> StoredBit -> StoredBit -> StoredBit ->
        Bool -> Bool -> Bool ->  Bool ->
        MonadCircuit (Bool, Bool, Bool)
-- Do straight copies when needed XXX
cell H01 bit0 bit1 bit2 bit3 bit4 ena data_ shiftInUp shiftInDown = do
        when ena $ bit0 @= data_
        bit2 @= shiftInDown
        bit4 @= shiftInUp
        return (False, False, False)
cell H02 bit0 bit1 bit2 bit3 bit4 ena data_ shiftInUp shiftInDown = do
        when ena $ bit0 @= data_
        bit2' <- previous bit2
        bit1 @= not bit2'
        bit3' <- previous bit3
        bit4 @= not bit3'
        return (False, False, False)

main :: IO ()
main = do
    print "Hello, World!"
