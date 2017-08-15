{-# LANGUAGE GADTs           #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Category
import           Control.Monad.Indexed

import           Data.Composition
import           Data.Distributive

import           Linear
import           Linear.V

import           Numeric.AD

import           Prelude               hiding (id, (.))

import           IndexedTardis

-- | The type representing a single sigmoid layer
data Layer f a b = Layer {weights :: V b (V a f), biases :: V b f}

-- | A network is a series of compatible layers
data Network f a b where
  Id :: Network f a a
  Lr :: (Dim a, Dim b) => Layer f a b -> Network f b c -> Network f a c

-- | Networks form a category in the same way lists form a monoid
instance Category (Network f) where
  id = Id
  m . Id = m
  m . Lr l n = Lr l (m . n)

-- | Left fold over a network. Apply a function layer by layer forwards through
-- the network.
--
-- If you don't want to have the type being folded to be paramtrised, or to be
-- parametrised in a different way, you can use wrapper types like Flip and
-- Const to introduce and move a parameter.
foldlNetwork :: (forall a b. (Dim a, Dim b) => v a -> Layer f a b -> v b) ->
  v a -> Network f a b -> v b
foldlNetwork f v Id       = v
foldlNetwork f v (Lr l n) = foldlNetwork f (f v l) n

-- | Right fold over a network. Apply a function layer by layer backwards
-- through the network.
foldrNetwork :: (forall a b. (Dim a, Dim b) => Layer f a b -> v b -> v a) ->
  v b -> Network f a b -> v a
foldrNetwork f v Id       = v
foldrNetwork f v (Lr l n) = f l (foldrNetwork f v n)

-- | Traverse a network using an 'IxApplicative'.
traverseNetwork :: IxApplicative ia =>
  (forall a b. (Dim a, Dim b) => Layer f a b -> ia a b (Layer g a b)) ->
  Network f a b -> ia a b (Network g a b)
traverseNetwork _ Id       = ireturn Id
traverseNetwork f (Lr l n) = Lr `imap` f l `iap` traverseNetwork f n

newtype Flip f b a = Flip {unflip :: f a b}

feedForward1 :: (Floating f, Dim a, Dim b) => (f -> f) ->
  V a f -> Layer f a b -> V b f
feedForward1 sigmoid i Layer{..} = sigmoid <$> weights !* i + biases

-- | Given a sigmoid function, for example the logistic function
-- @\x -> 1 / (1 + exp (- x))@, or @tanh@ or @atan@, feed an input vector
-- through the network.
feedForward :: Floating f => (f -> f) -> V a f -> Network f a b -> V b f
feedForward sigmoid =
  unflip .: foldlNetwork (Flip .: feedForward1 sigmoid . unflip) . Flip

backPropogate1 :: (Floating f, Dim a, Dim b) =>
  (forall f. Floating f => f -> f) ->
  Layer f a b -> ITardis (Flip V f) (Flip V f) a b (Layer f a b)
backPropogate1 sigmoid Layer{..} = ITardis $ \(Flip db, Flip za) ->
  let aa = sigmoid za
      zb = weights !* aa + biases
      da = transpose weights !* db * diff sigmoid za
  in (Layer {weights = weights + db `outer` aa,
             biases  = biases + db},
      (Flip da, Flip zb))

-- | Given a pair of input and expected output, a sigmoid function, and a cost
-- function, for example @qd@, run the backpropogation algorithm for a network
-- and a single input.
backPropogate' :: (Dim b, Floating f) => (V a f, V b f) ->
  (forall f. Floating f => f -> f) ->
  (forall f. Floating f => V b f -> V b f -> f) ->
  Network f a b -> Network f a b
backPropogate' (x, y) sigmoid cost network =
  let (network', (_, Flip zl)) = runITardis
        (traverseNetwork (backPropogate1 sigmoid) network)
        (Flip dl, Flip x)
      al = sigmoid zl
      dl = grad (cost $ fmap auto y) al * diff sigmoid zl
  in network'

-- | 'backPropogate\'' with a sensible default sigmoid and cost.
backPropogate :: (Dim b, Floating f) => (V a f, V b f) ->
  Network f a b -> Network f a b
backPropogate (x, y) = backPropogate' (x, y) logistic qd
  where
    logistic x = 1 / (1 + exp (- x))

batch :: Dim w => Network f a b -> Network (V w f) a b
batch Id = Id
batch (Lr Layer{..} n) = Lr l' (batch n)
  where
    l' = Layer {weights = fmap (fmap pure) weights, biases = fmap pure biases}

unbatch :: (Dim w, Fractional f) => Network (V w f) a b -> Network f a b
unbatch Id = Id
unbatch (Lr Layer{..} n) = Lr l' (unbatch n)
  where
    l' = Layer {weights = fmap (fmap average) weights,
                biases  = fmap average biases}
    average v = sum v / fromIntegral (dim v)

runMiniBatch' :: (Dim w, Dim a, Dim b, Floating f) => V w (V a f, V b f) ->
  (forall f. Floating f => f -> f) ->
  (forall f. Floating f => V b f -> V b f -> f) ->
  Network f a b -> Network f a b
runMiniBatch' xys sigmoid cost =
  unbatch .
  backPropogate' (collect fst xys, collect snd xys) sigmoid cost .
  batch

runMiniBatch :: (Dim w, Dim a, Dim b, Floating f) => V w (V a f, V b f) ->
  Network f a b -> Network f a b
runMiniBatch xys =
  unbatch .
  backPropogate (collect fst xys, collect snd xys) .
  batch

main :: IO ()
main = putStrLn "Hello, Haskell!"
