{-# LANGUAGE RecordWildCards, RecursiveDo #-}

module Widgets.Cast where

import Widgets.Core
import Widgets.Links
import Widgets.Obscura
import Widgets.Ranger
import Data.List.Split

class Cast m where
    userActive :: m -> Tidings Int

data Case = Case { _ecase :: ECase, _actuateCE :: Tidings Int }
data Cask = Cask { _ecask :: ECask, _actuateCK :: Tidings Int }

data ECase = ECase { _elemCE :: [Link Int] }
data ECask = ECask { _elemCK :: [Link Int] }

instance Widget ECase where
  widget ecase = row 5 $ map widget (_elemCE row)

instance Cast Case where
    userActive = _actuateCE

-- instance Widget ECask where

instance Cast Cask where
    userActive = _actuateCK

blTranspose :: Int -> a -> Behavior [a] -> [Behavior a]
blTranspose n z bxs = map (\x -> (!!x) <$> (take n . (++(repeat z)) <$> bxs)) (enumFromTo 0 (n-1))

lbTranspose :: [Behavior a] -> Behavior [a]
lbTranspose = foldr (\x acc -> (:) <$> x <*> acc) (pure [])


-- | Cast builder for Cases only, which relies on a static list to
-- display sections of; the page size is an FR (possibly pure) value
softCast :: [a] -- ^ Full list
         -> Behavior Int -- ^ Number per page
         -> (a -> String) -- ^ Label for the softlinks
         -> (a -> SoftLink Int -> UI Element) -- ^ Row transformer for items
         -> UI Case
softCast lFull bBiteSize label fRower = do
    let bits = length lFull
        values = [0..(bits-1)]
        bBites = (bits`cdiv`) <$> bBiteSize

    rec range <- ranger bThis bFirst bLast (pure (string.show.succ))
        let tRanger = userLoc range
            eRanger = rumors tRanger
            bRanger = facts tRanger
            bFirst = pure 0
            bLast = pred <$> bBites
        bThis <- stepper 0 $ eRanger

    softs <- sequence (zipWith (softLink) (map (label) lFull) (values))

    let bChunks = chunksOf <$> bBiteSize <*> (pure softs)
        eSofts = (map (rumors.tideLink) softs)
        bSofts = (!!) <$> bChunks <*> bThis
        eActua = head <$> unions (eSofts++[ (-1) <$ eRanger ])

    softBox <- UI.table
    element softBox # sink (mapkinder (\(f,x) -> f $ x)) (zip (map fRower lFull) <$> bSofts)

    wrapper <- column [ row [element softBox ], row [element range] ]

    let _elementCE = wrapper
        _actuateCE = tidings (pure (-1)) $ eActua
    return Case{..}

-- | Cast builder for Casks only, which relies on an FR (possibly pure) list to display
-- sections of; the page size must be a pure integer value
liquidCast :: Behavior [a] -- ^ Full list
           -> Int -- ^ Number per page (cannot be FR)
           -> (a -> String) -- ^ Label for the liquidlinks
           -> (a -> (LiquidLink Int -> UI Element)) -- ^ Row transformer for items
           -> UI Cask
liquidCast bFull biteSize label fRower = do
    let values = (map fst . zip [0..] <$> bFull)
        bBits = length <$> values
        bBites = ((`cdiv`biteSize) <$> bBits)

    rec range <- ranger bThis bFirst bLast (pure (string.show.succ))
        let tRanger = userLoc range
            eRanger = rumors tRanger
            bRanger = facts tRanger
            bFirst = pure 0
            bLast = pred <$> bBites
        bThis <- stepper 0 $ eRanger

    let
        bChunks = chunksOf biteSize <$> values
        bValues = blTranspose biteSize (-1) ((!!) <$> bChunks <*> bThis)

    liquids <- sequence (zipWith liquidLink (replicate biteSize ((.) <$> (pure label) <*> (((.abs).(!!)) <$> bFull))) bValues)

    let eLiquids = (map (rumors.tideLink) liquids)
        eActua = head <$> unions (eLiquids++[ (-1) <$ eRanger])

    liquidBox <- UI.table
    element liquidBox # sink (mapkinder (\(f,x) -> f $ x)) (zip <$> (map fRower <$> ((!!) <$> (chunksOf biteSize <$> bFull) <*> bThis)) <*> (filtrate <$> (lbTranspose $ map (((>=0) <$>).getFlux) liquids) <*> (pure liquids)))
    wrapper <- column [ row [ element liquidBox ], row [element range] ]

    let _elementCK = wrapper
        _actuateCK = tidings (pure (-1)) $ eActua
    return Cask{..}


derangedCask :: Behavior [a] -- ^ Full list
           -> Int -- ^ Number per page (cannot be FR)
           -> Ranger Int -- ^ External ranger
           -> Behavior (a -> String) -- ^ Label for the liquidlinks
           -> Behavior (a -> LiquidLink Int -> UI Element) -- ^ Row transformer for items
           -> Behavior ([UI Element] -> [UI Element]) -- ^ Row combiner for items
           -> UI Cask
derangedCask bFull biteSize range bLabel bRower bCombo = do
    let values = (map fst . zip [0..] <$> bFull)
        bBits = length <$> values
        bBites = ((`cdiv`biteSize) <$> bBits)
        tRanger = userLoc range
        eRanger = rumors tRanger
        bRanger = facts tRanger
        bFirst :: Behavior Int
        bFirst = pure 0
        bLast = pred <$> bBites
    bThis <- stepper 0 $ eRanger

    let
        bChunks = chunksOf biteSize <$> values
        bValues = blTranspose biteSize (-1) ((!!) <$> bChunks <*> bThis)

    liquids <- sequence (zipWith liquidLink (replicate biteSize ((.) <$> bLabel <*> (((.abs).(!!)) <$> bFull))) bValues)

    let eLiquids = (map (rumors.tideLink) liquids)
        eActua = head <$> unions (eLiquids++[ (-1) <$ eRanger])

    liquidBox <- UI.table
    element liquidBox # sink (kinder (\(f,xs) -> f $ xs)) ((,) <$> bCombo <*> (zipWith ($) <$> (map <$> bRower <*> ((!!) <$> (chunksOf biteSize <$> bFull) <*> bThis)) <*> (filtrate <$> (lbTranspose $ map (((>=0) <$>).getFlux) liquids) <*> (pure liquids))))

    let _elementCK = liquidBox
        _actuateCK = tidings (pure (-1)) $ eActua
    return Cask{..}


oculus :: Behavior [a] -- ^ Full list
       -> Int -- ^ Number per page (cannot be FR)
       -> Ranger Int -- ^ External ranger
       -> Behavior (a -> String) -- ^ URL for the obscurae
       -> Behavior (a -> LiquidLink Int -> UI Element) -- ^ Row transformer for items
       -> Behavior ([UI Element] -> [UI Element]) -- ^ Row combiner for items
       -> UI (Cask, [LiquidLink Int])
oculus bFull biteSize range bLinker bRower bCombo = do
    let values = (map fst . zip [0..] <$> bFull)
        bBits = length <$> values
        bBites = ((`cdiv`biteSize) <$> bBits)
        tRanger = userLoc range
        eRanger = rumors tRanger
        bRanger = facts tRanger
        bFirst :: Behavior Int
        bFirst = pure 0
        bLast = pred <$> bBites
    bThis <- stepper 0 $ eRanger

    let
        bChunks = chunksOf biteSize <$> values
        bValues = blTranspose biteSize (-1) ((!!) <$> bChunks <*> bThis)

    liquids <- sequence (zipWith obscura (replicate biteSize ((.) <$> bLinker <*> (((.abs).(!!)) <$> bFull))) bValues)

    let eLiquids = (map (rumors.tideLink) liquids)
        eActua = head <$> unions (eLiquids++[ (-1) <$ eRanger])

    liquidBox <- UI.table
    element liquidBox # sink (kinder (\(f,xs) -> f $ xs)) ((,) <$> bCombo <*> (zipWith ($) <$> (map <$> bRower <*> ((!!) <$> (chunksOf biteSize <$> bFull) <*> bThis)) <*> (filtrate <$> (lbTranspose $ map (((>=0) <$>).getFlux) liquids) <*> (pure liquids))))

    let _elementCK = liquidBox
        _actuateCK = tidings (pure (-1)) $ eActua
    return (Cask{..}, liquids)
