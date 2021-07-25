{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Polysemy.Vinyl
-- License     : MIT
-- Maintainer  : dan.firth@homotopic.tech
-- Stability   : experimental
--
-- Extra functions for using vinyl records with polysemy.
module Polysemy.Vinyl
  ( rContramapInput,
    rContramapInput',
    rMapOutput,
    rMapOutput',
    separateRecInput,
    separateRecInput',
    stripRecInput,
    endRecInput,
    runInputConstFC,
    runSeveral,
  )
where

import Control.Applicative
import Control.Arrow
import Data.Kind
import Data.Vinyl
import Data.Vinyl.Functor
import Polysemy
import Polysemy.Extra
import Polysemy.Input
import Polysemy.Output
import Polysemy.Several hiding (runSeveral)

-- | Map an `Input` containing a `Rec` contravariantly via a natural transformation.
-- Uses `rmap`.
--
-- @since 0.1.0.0
rContramapInput ::
  (RMap xs, Members '[Input (Rec f xs)] r) =>
  -- | A natural transformation from f to g.
  (forall y. f y -> g y) ->
  Sem (Input (Rec g xs) ': r) a ->
  Sem r a
rContramapInput k = contramapInput (rmap k)
{-# INLINE rContramapInput #-}

-- | Reinterpreting version of `rContramapInput`.
--
-- @since 0.1.0.0
rContramapInput' ::
  RMap xs =>
  -- | A natural transformation from f to g.
  (forall y. f y -> g y) ->
  Sem (Input (Rec g xs) ': r) a ->
  Sem (Input (Rec f xs) ': r) a
rContramapInput' k = raiseUnder >>> rContramapInput k
{-# INLINE rContramapInput' #-}

-- | Map an `Output` containing a `Rec` covariantly via a natural transformation.
-- Uses `rmap`.
--
-- @since 0.1.0.0
rMapOutput ::
  (RMap xs, Members '[Output (Rec g xs)] r) =>
  -- | A natural transformation from f to g.
  (forall y. f y -> g y) ->
  Sem (Output (Rec f xs) ': r) a ->
  Sem r a
rMapOutput k = mapOutput (rmap k)
{-# INLINE rMapOutput #-}

-- | Reinterpreting version of `rMapOutput`.
--
-- @since 0.1.0.0
rMapOutput' ::
  RMap xs =>
  -- | A natural transformation from f to g.
  (forall y. f y -> g y) ->
  Sem (Output (Rec f xs) ': r) a ->
  Sem (Output (Rec g xs) ': r) a
rMapOutput' k = raiseUnder >>> rMapOutput k
{-# INLINE rMapOutput' #-}

-- | Separate one of the fields of an `Input` `Rec` into its own `Input`.
--
-- @since 0.1.2.0
separateRecInput ::
  forall f x xs r a.
  Members
    '[ Input (Rec f xs),
       Input (f x)
     ]
    r =>
  Sem (Input (Rec f (x ': xs)) ': r) a ->
  Sem r a
separateRecInput = interpret \case
  Input -> liftA2 (:&) (input @(f x)) (input @(Rec f xs))
{-# INLINE separateRecInput #-}

-- | Reinterpreting version of `separateRecInput`. This assumes you want to handle
-- the separated case first.
--
-- @since 0.1.2.0
separateRecInput' ::
  forall f x xs r a.
  Sem (Input (Rec f (x ': xs)) ': r) a ->
  Sem (Input (f x) ': Input (Rec f xs) ': r) a
separateRecInput' = reinterpret2 \case
  Input -> liftA2 (:&) (input @(f x)) (raise $ input @(Rec f xs))
{-# INLINE separateRecInput' #-}

-- | Like `separateRecInput`, but places the remainer of the `Rec` at the head
-- of the list while pushing the case into the stack. This is useful when you
-- want to eliminate the record first by repeated applications of `stripRecInput`.
--
-- @since 0.1.2.0
stripRecInput ::
  forall f x xs r a.
  Members '[Input (f x)] (Input (Rec f xs) ': r) =>
  Sem (Input (Rec f (x ': xs)) ': r) a ->
  Sem (Input (Rec f xs) ': r) a
stripRecInput = reinterpret \case
  Input -> liftA2 (:&) (input @(f x)) (input @(Rec f xs))
{-# INLINE stripRecInput #-}

-- | Discard a depleted `Rec` `Input` by returning `RNil`.
--
-- @since 0.1.2.0
endRecInput :: Sem (Input (Rec f '[]) ': r) a -> Sem r a
endRecInput = interpret \case
  Input -> return RNil
{-# INLINE endRecInput #-}

-- | Like `runInputConstF` but for vinyl composed functors.
--
-- @since 0.1.3.0
runInputConstFC ::
  forall b f g r a.
  f (g b) ->
  Sem (Input ((f :. g) b) ': r) a ->
  Sem r a
runInputConstFC f = runInputConstF @b @(f :. g) (Compose f)
{-# INLINE runInputConstFC #-}

-- | Like `Polysemy.Several.runSeveral` but for a vinyl `Rec`.
--
-- @since 0.1.5.0
runSeveral ::
  forall (e :: Type -> Effect) f (r :: [Effect]) xs a.
  (forall r' k x. k -> Sem (e k ': r') x -> Sem r' x) ->
  Rec f xs ->
  Sem (Append (TypeMap e (TypeMap f xs)) r) a ->
  Sem r a
runSeveral f (a :& as) = runSeveral f as . f a
runSeveral _ RNil = id
{-# INLINE runSeveral #-}
