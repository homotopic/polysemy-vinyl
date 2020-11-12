{-|
Module      : Polysemy.Vinyl
License     : MIT
Maintainer  : dan.firth@homotopic.tech
Stability   : experimental

Extra functions for using vinyl records with polysemy.
-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Polysemy.Vinyl (
  rContramapInput
, rContramapInput'
, rMapOutput
, rMapOutput'
) where

import Control.Arrow
import Data.Vinyl
import Polysemy
import Polysemy.Extra
import Polysemy.Input
import Polysemy.Output

-- | Map an `Input` containing a `Rec` contravariantly via a natural transformation.
-- Uses `rmap`.
--
-- @since 0.1.0.0
rContramapInput :: (RMap xs, Members '[Input (Rec f xs)] r)
                => (forall y. f y -> g y)
                   -- ^ A natural transformation from f to g.
                -> Sem (Input (Rec g xs) ': r) a
                -> Sem r a
rContramapInput k = contramapInput (rmap k)

-- | Reinterpreting version of `rContramapInput`.
--
-- @since 0.1.0.0
rContramapInput' :: RMap xs
                 => (forall y. f y -> g y)
                    -- ^ A natural transformation from f to g.
                 -> Sem (Input (Rec g xs) ': r) a
                 -> Sem (Input (Rec f xs) ': r) a
rContramapInput' k = raiseUnder >>> rContramapInput k

-- | Map an `Output` containing a `Rec` covariantly via a natural transformation.
-- Uses `rmap`.
--
-- @since 0.1.0.0
rMapOutput :: (RMap xs, Members '[Output (Rec g xs)] r)
           => (forall y. f y -> g y)
              -- ^ A natural transformation from f to g.
           -> Sem (Output (Rec f xs) ': r) a
           -> Sem r a
rMapOutput k = mapOutput (rmap k)

-- | Reinterpreting version of `rMapOutput`.
--
-- @since 0.1.0.0
rMapOutput' :: RMap xs
            => (forall y. f y -> g y)
               -- ^ A natural transformation from f to g.
            -> Sem (Output (Rec f xs) ': r) a
            -> Sem (Output (Rec g xs) ': r) a
rMapOutput' k = raiseUnder >>> rMapOutput k
