{-# LANGUAGE PolyKinds #-}
module WrapIndex where

import Control.Monad.Indexed

newtype IWrap m i j a = IWrap {iunwrap :: m a}

instance Functor m => IxFunctor (IWrap m) where
  imap f = IWrap . fmap f . iunwrap

instance Applicative m => IxPointed (IWrap m) where
  ireturn = IWrap . pure

instance Applicative m => IxApplicative (IWrap m) where
  IWrap f `iap` IWrap x = IWrap $ f <*> x

instance Monad m => IxMonad (IWrap m) where
  f `ibind` IWrap x = IWrap $ x >>= iunwrap . f
