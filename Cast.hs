{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Widgets.Cast where

import Widgets.Core hiding (wrap, Row, Table, label)
import Widgets.Links
import Widgets.Obscura
import Widgets.Ranger
import Widgets.Table
import Data.List.Split
import Util hiding (visible)

data Cast = Cast { _cast :: ECast
                 , _actuate :: Tidings Int }

data ECast = ECast { _elem :: Tabular }

instance Widget ECast where
  widget = widget . _table . _elem

instance Courier Cast Int where
  type Element Cast = ECast
  element = _cast
  tide = _actuate

blTranspose :: Int -> a -> Behavior [a] -> [Behavior a]
blTranspose n z bxs = map (\x -> (!!x) <$> (take n . (++(repeat z)) <$> bxs)) (enumFromTo 0 (n-1))

lbTranspose :: [Behavior a] -> Behavior [a]
lbTranspose = foldr (\x acc -> (:) <$> x <*> acc) (pure [])


type Label a = StaticDynamic (a -> String)
type Wrapper a = StaticDynamic (a -> Link Int -> Row)
type Collector = StaticDynamic ([Row] -> [Row])

data Format a = Format { label :: Label a, wrap :: Wrapper a, collect :: Collector }

data CastType a = Cask { bContents :: Behavior [a],  pagesize ::          Int, current :: Tidings Int, format :: Format a }
                | Case { contents  ::          [a], bPagesize :: Behavior Int, current :: Tidings Int, format :: Format a }


bFinal :: CastType a -> Behavior Int
bFinal x = pred <$> bFinal' x
  where
    bFinal' (Cask{..}) = (`cdiv`pagesize) . length <$> bContents
    bFinal' (Case{..}) =   cdiv  (length contents) <$> bPagesize

bIndexChunks :: CastType a -> Behavior [[Int]]
bIndexChunks x = case x of
                   (Cask{..}) -> chunksOf pagesize . evaluate <$> bContents
                   (Case{..}) -> flip chunksOf (evaluate contents) <$> bPagesize
  where
    evaluate :: forall a. [a] -> [Int]
    evaluate = map fst . zip [0..]

bValueChunks :: CastType a -> Behavior [[a]]
bValueChunks (Cask{..}) = chunksOf pagesize <$> bContents
bValueChunks (Case{..}) = flip chunksOf contents <$> bPagesize


sdIndexLabel :: CastType a -> StaticDynamic (Int -> String)
sdIndexLabel c@(Cask{..}) = Dynamic $ (.) <$> bLabor <*> bIndex
  where
    bLabor = flux . label $ format
    bIndex = (\xs i -> xs !! (abs i)) <$> bContents
sdIndexLabel c@(Case{..}) = Static $ labor . index
  where
    labor = freeze . label $ format
    index = (contents!!)

genCast :: CastType a -> Table -> MomentIO Cast
genCast x@(Cask{..}) tab = mdo
    let
        indexChunks :: Behavior [[Int]] -- ^ values, chunked by pagesize
        indexChunks = bIndexChunks x
        bIndices :: [Behavior Int] -- ^ collated values in the current chunk
        bIndices = blTranspose pagesize (-1) $ (!!) <$> indexChunks <*> (facts current)
        bLabelon :: Behavior (Int -> String) -- ^ label converter: index to value-string
        bLabelon = flux . sdIndexLabel $ x
        secrete :: Behavior Int -> MomentIO (Link Int)
        secrete y = (liftIO $ preLink tab) >>= \l -> liquidLink l bLabelon y
    liquids <- sequence . map secrete $ bIndices
    let
        eLiquids = map portents liquids
        eActua = priorityUnion (((-1) <$ rumors current):eLiquids)
        sdCollect = collect $ format
        sdWrap = wrap $ format
    let
        valueChunks = bValueChunks x
        bValues = (!!) <$> valueChunks <*> (facts current)
        bLinks :: Behavior [Link Int]
        bLinks = (`filtrate`liquids) <$> (lbTranspose $ map (((>=0) <$>).(flux.rubicon)) liquids)
        rawRows :: Behavior [Row]
        rawRows = zipWith <$> flux sdWrap <*> bValues <*> bLinks
    liquidBox <- tabulate tab $ combine sdCollect rawRows

    let _cast = ECast liquidBox
        _actuate = tidings (pure (-1)) $ eActua
    return Cast{..}
genCast x@(Case{..}) tab = mdo
    let
        indices :: [Int]
        indices = map fst . zip [0..] $ contents
        label :: Int -> String
        label = freeze . sdIndexLabel $ x
        bIndex :: Behavior [[Int]]
        bIndex = (`chunksOf`indices) <$> bPagesize
        bShows :: Behavior ([Row] -> [Bool])
        bShows = verity <$> ((!!) <$> bIndex <*> facts current)
        valueChunks = bValueChunks x
        curValues = (!!) <$> valueChunks <*> facts current
        extrude :: Int -> MomentIO (Link Int)
        extrude y =
          do { l <- liftIO $ preLink tab;
               sink l [ visible :== (!!y) <$> bShow ];
               softLink l label y;
             }

    softs <- sequence . map extrude $ indices

    let
        bChunks :: Behavior [[Link Int]]
        bChunks = (`chunksOf`softs) <$> bPagesize
        eSofts :: [Event Int]
        eSofts = map blood softs
        bSofts :: Behavior [Link Int]
        bSofts = (!!) <$> bChunks <*> (facts current)
        eActua :: Event Int
        eActua = priorityUnion (((-1) <$ rumors current):eSofts)
        bAllRows :: Behavior [Row]
        bAllRows = zipWith <$> (flux . wrap $ format) <*> (concat <$> valueChunks) <*> (concat <$> bChunks)
        bShow :: Behavior [Bool]
        bShow = bShows <*> bAllRows
        bRows :: Behavior [Row]
        bRows = zipWith <$> (flux . wrap $ format) <*> curValues <*> bSofts

    softBox <- tabulate tab $ combine (collect format) bRows
    --redrawRows bShows bAllRows


    let _cast = ECast softBox
        _actuate = tidings (pure (-1)) $ eActua
    return Cast{..}

{-
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
-}
