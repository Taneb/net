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

