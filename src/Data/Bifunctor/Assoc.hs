module Data.Bifunctor.Assoc (
    Assoc (..),
    assoc', unassoc',
    ) where

import Data.Bifunctor (Bifunctor (..))
import Data.Bifunctor.Flip (Flip (..))
import Data.Bifunctor.Tannen (Tannen (..))
import Data.Bifunctor.Product (Product (..))

-- | "Semigroup-y" 'Bifunctor's.
--
-- @
-- 'assoc' . 'unassoc' = 'id'
-- 'unassoc' . 'assoc' = 'id'
-- 'assoc' . 'bimap' ('bimap' f g) h = 'bimap' f ('bimap' g h) . 'assoc'
-- @
--
-- This library doesn't provide @Monoidal@ class, with left and right unitors.
-- Are they useful in practice?
--
-- /Note:/ 'assoc' and 'unassoc' use irrefutable pattern matches when possible.
--
class Bifunctor p => Assoc p where
    assoc :: p (p a b) c -> p a (p b c)
    unassoc :: p a (p b c) -> p (p a b) c

instance Assoc (,) where
    assoc ~((a, b), c) = (a, (b, c))
    unassoc ~(a, (b, c)) = ((a, b), c)

instance Assoc Either where
    assoc (Left (Left a))  = Left a
    assoc (Left (Right b)) = Right (Left b)
    assoc (Right c)        = Right (Right c)

    unassoc (Left a)          = Left (Left a)
    unassoc (Right (Left b))  = Left (Right b)
    unassoc (Right (Right c)) = Right c

instance Assoc p => Assoc (Flip p) where
    assoc   = Flip . first Flip . unassoc . second runFlip . runFlip
    unassoc = Flip . second Flip . assoc . first runFlip . runFlip

-- $setup
--
-- TODO: make proper test-suite
--
-- >>> import Data.Proxy
-- >>> import Test.QuickCheck
-- >>> import Data.Functor.Classes
--
-- >>> :{
--     let assocUnassocLaw :: (Assoc p, Eq2 p) => Proxy p -> p Bool (p Int Char) -> Bool
--         assocUnassocLaw _ x = liftEq2 (==) eq2 (assoc (unassoc x)) x
--     :}
--
-- >>> quickCheck $ assocUnassocLaw (Proxy :: Proxy (,))
-- +++ OK, passed 100 tests.
--
-- >>> quickCheck $ assocUnassocLaw (Proxy :: Proxy Either)
-- +++ OK, passed 100 tests.
--
-- >>> :{
--     let unassocAssocLaw :: (Assoc p, Eq2 p) => Proxy p -> p (p Int Char) Bool -> Bool
--         unassocAssocLaw _ x = liftEq2 eq2 (==) (unassoc (assoc x)) x
--     :}
--
-- >>> quickCheck $ unassocAssocLaw (Proxy :: Proxy (,))
-- +++ OK, passed 100 tests.
--
-- >>> quickCheck $ unassocAssocLaw (Proxy :: Proxy Either)
-- +++ OK, passed 100 tests.
--
-- >>> :{
--     let bimapLaw :: (Assoc p, Eq2 p) => Proxy p
--                  -> Fun Int Char -> Fun Char Bool -> Fun Bool Int
--                  -> p (p Int Char) Bool
--                  -> Bool
--         bimapLaw _ (Fun _ f) (Fun _ g) (Fun _ h) x = liftEq2 (==) eq2
--             (assoc . bimap (bimap f g) h $ x)
--             (bimap f (bimap g h) . assoc $ x)
--     :}
--
-- >>> quickCheck $ bimapLaw (Proxy :: Proxy (,))
-- +++ OK, passed 100 tests.
--
-- >>> quickCheck $ bimapLaw (Proxy :: Proxy Either)
-- +++ OK, passed 100 tests.
--

-- | Strict 'assoc'.
assoc' :: Assoc p => p (p a b) c -> p a (p b c)
assoc' x = x `seq` assoc x

-- | Strict 'unassoc'.
unassoc' :: Assoc p => p a (p b c) -> p (p a b) c
unassoc' x = x `seq` unassoc x
