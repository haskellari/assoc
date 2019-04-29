{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.Inspection (inspect, (===))
import Control.Lens (Iso, iso, view, review)
import Data.Bifunctor.Swap
import GHC.Exts (inline)

pairLHS, pairRHS :: (a, b) -> (b, a)
pairLHS x = inline swap x
pairRHS ~(x, y) = (y, x)

inspect $ 'pairLHS === 'pairRHS

pairLHS', pairRHS' :: (a, b) -> (b, a)
pairLHS' = swap'
pairRHS' (x, y) = (y, x)

inspect $ 'pairLHS' === 'pairRHS'

swapped :: Iso (a,b) (c,d) (b,a) (d,c)
swapped = iso swap swap

swappedLHS', swappedRHS' :: Iso (a,b) (c,d) (b,a) (d,c)
swappedLHS' = iso swap' swap'
swappedRHS' = iso f g where
  f x = x `seq` view swapped x
  g y = y `seq` review swapped y

inspect $ 'swappedLHS' === 'swappedRHS'

main :: IO ()
main = return ()
