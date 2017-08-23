module IndexedTardis where

import           Control.Monad.Indexed
import           Control.Monad.Indexed.Fix

newtype ITardis b f i j a = ITardis {runITardis :: (b j, f i) -> (a, (b i, f j))}

instance IxFunctor (ITardis b f) where
  imap k (ITardis tx) = ITardis $ \s -> let (x, t) = tx s in (k x, t)

instance IxPointed (ITardis b f) where
  ireturn x = ITardis $ \s -> (x, s)

instance IxApplicative (ITardis b f) where
  iap (ITardis tk) (ITardis tx) = ITardis $ \(bk, fi) ->
    let (k, (bi, fj)) = tk (bj, fi)
        (x, (bj, fk)) = tx (bk, fj)
    in (k x, (bi, fk))

instance IxMonad (ITardis b f) where
  ibind k (ITardis tx) = ITardis $ \(bk, fi) ->
    let (x, (bi, fj)) = tx (bj, fi)
        (y, (bj, fk)) = runITardis (k x) (bk, fj)
    in (y, (bi, fk))

instance IxMonadFix (ITardis b f) where
  imfix xtx = ITardis $ \(bj, fi) ->
    let (x, (bi, fj)) = runITardis (xtx x) (bj, fi)
    in (x, (bi, fj))

getPast :: ITardis b f i i (f i)
getPast = ITardis $ \(b, f) -> (f, (b, f))

getFuture :: ITardis b f i i (b i)
getFuture = ITardis $ \(b, f) -> (b, (b, f))

sendPast :: b i -> ITardis b f i i ()
sendPast b = ITardis $ \(_, f) -> ((), (b, f))

sendFuture :: f i -> ITardis b f i i ()
sendFuture f = ITardis $ \(b, _) -> ((), (b, f))

getBoth :: ITardis b f i i (b i, f i)
getBoth = ITardis $ \s -> (s, s)

sendBoth :: (b i, f j) -> ITardis b f i j ()
sendBoth s = ITardis $ \_ -> ((), s)

modifyForwards :: (f i -> f i) -> ITardis b f i i ()
modifyForwards k = ITardis $ \(b, f) -> ((), (b, k f))

modifyBackwards :: (b i -> b i) -> ITardis b f i i ()
modifyBackwards k = ITardis $ \(b, f) -> ((), (k b, f))

modifyBoth :: (b j -> b i) -> (f i -> f j) -> ITardis b f i j ()
modifyBoth kb kf = ITardis $ \(b, f) -> ((), (kb b, kf f))
