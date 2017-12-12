{-# LANGUAGE RecursiveDo #-}
module Reactive.Util where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Monad.Fix
{-
import qualified Reactive.Banana.Internal.Combinators as Prim
import Reactive.Banana.Types
-}


-- | anyEvent
-- Combine a list of unit-valued events into a union event that triggers whenever any element does
anyEvent :: [Event ()] -> Event ()
anyEvent = foldl1 (unionWith (\_ _ -> ()))


-- | priorityUnion
-- Combine a list of events, giving earlier elements of the list higher precedence on simultaneous triggers
priorityUnion :: [Event a] -> Event a
priorityUnion = foldl1 (unionWith const)

-- | aggregate
-- Behavior constructor that accumulates IO-monad results calculated on event triggers in a list, most recent first
aggregate :: [b] -> (a -> IO b) -> Event a -> MomentIO (Behavior [b])
aggregate bs new eAdd = mdo
    eAddition <- mapEventIO new eAdd
    let eAll = flip (:) <$> bAll <@> eAddition
    bAll <- stepper bs eAll
    return bAll


-- | restepper
-- Create a Behavior that uses its current value when an event is triggered to accumulate state
restepper :: b -> (b -> a -> b) -> Event a -> MomentIO (Behavior b)
restepper z f e = mdo
    let eDelta = f <$> bDelta <@> e
    bDelta <- stepper z eDelta
    return bDelta

-- | override
-- Returns an event that conditionally replaces trigger values with a time-varying value, based on a time-varying boolean predicate
override :: Behavior Bool -> Behavior a -> Event a -> Event a
override on feed source = priorityUnion [ feed <@ whenE on source
                                        , source
                                        ]
