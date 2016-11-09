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

-- | Widget consisting of a paged table of widgets containing buttons, which keeps track of the
-- index of widgets as their buttons are clicked
data Cast = Cast { _cast :: ECast
                 , _actuate :: Tidings Int }

-- | Element component of the Cast widget
data ECast = ECast { _elem :: Tabular }

instance Widget ECast where
  widget = widget . _table . _elem

instance Courier Cast Int where
  type Element Cast = ECast
  element = _cast
  tide = _actuate

-- | Given a list length and a default value, transform a @Behavior [a]@ to a $[Behavior a]$, with
-- any out-of-range elements being the constant behavior containing the default value
blTranspose :: Int -> a -> Behavior [a] -> [Behavior a]
blTranspose n z bxs = map (\x -> (!!x) <$> (take n . (++(repeat z)) <$> bxs)) (enumFromTo 0 (n-1))

-- | Transform lists of Behaviors into Behaviors of lists
lbTranspose :: [Behavior a] -> Behavior [a]
lbTranspose = foldr (\x acc -> (:) <$> x <*> acc) (pure [])


type Label a = StaticDynamic (a -> String)
type Wrapper a = StaticDynamic (a -> Link Int -> Row)
type Collector = StaticDynamic ([Row] -> [Row])

-- | Definition of the elemental layout of a cast, determining how each item is ultimately displayed
data Format a = Format { label :: Label a, wrap :: Wrapper a, collect :: Collector }

-- | Definition of the functional content of a cast, determining the items overall and which are in
-- focus at any given time, as well as the format definition
data CastType a = Cask { bContents :: Behavior [a],  pagesize ::          Int, current :: Tidings Int, format :: Format a }
                | Case { contents  ::          [a], bPagesize :: Behavior Int, current :: Tidings Int, format :: Format a }


bFinal :: CastType a -> Behavior Int
bFinal x = pred <$> bFinal' x
  where
    bFinal' (Cask{..}) = (`cdiv`pagesize) . length <$> bContents
    bFinal' (Case{..}) =   cdiv  (length contents) <$> bPagesize

-- | Given a CastType, generate a list of index-lists, each list corresponding to a page and each
-- item to a single index
bIndexChunks :: CastType a -> Behavior [[Int]]
bIndexChunks x = case x of
                   (Cask{..}) -> chunksOf pagesize . evaluate <$> bContents
                   (Case{..}) -> flip chunksOf (evaluate contents) <$> bPagesize
  where
    evaluate :: forall a. [a] -> [Int]
    evaluate = map fst . zip [0..]

-- | Given a CastType, partition its contents into page-sized lists
bValueChunks :: CastType a -> Behavior [[a]]
bValueChunks (Cask{..}) = chunksOf pagesize <$> bContents
bValueChunks (Case{..}) = flip chunksOf contents <$> bPagesize

-- | Extract the (possibly reactive) index-to-label function for a Cast
sdIndexLabel :: CastType a -> StaticDynamic (Int -> String)
sdIndexLabel c@(Cask{..}) = Dynamic $ (.) <$> bLabor <*> bIndex
  where
    bLabor = flux . label $ format
    bIndex = (\xs i -> xs !! (abs i)) <$> bContents
sdIndexLabel c@(Case{..}) = Static $ labor . index
  where
    labor = freeze . label $ format
    index = (contents!!)

-- | Generate a generic Cast from a CastType and the table frame it will occupy
genCast :: CastType a -> Table -> MomentIO Cast
genCast x@(Cask{..}) tab = mdo
    let
        indexChunks :: Behavior [[Int]] -- ^ values, chunked by pagesize
        indexChunks = bIndexChunks x
        bIndices :: [Behavior Int] -- ^ collated values in the current chunk
        bIndices = blTranspose pagesize (-1) $ (!!) <$> indexChunks <*> (facts current)
        bLabelon :: Behavior (Int -> String) -- ^ label converter: index to value-string
        bLabelon = flux . sdIndexLabel $ x
        secrete :: Behavior Int -> MomentIO (Link Int) -- ^ convert the reactive index in a static position to its liquidlink
        secrete y = (liftIO $ preLink tab) >>= \l -> liquidLink l bLabelon y
    -- generate the full complement of liquidlinks for the table
    liquids <- sequence . map secrete $ bIndices
    let
        eLiquids = map portents liquids
        -- unify the index-events of all liquidlinks in the table
        eActua = priorityUnion (((-1) <$ rumors current):eLiquids)
        sdCollect = collect $ format
        sdWrap = wrap $ format
    let
        valueChunks = bValueChunks x
        -- current range of values
        bValues = (!!) <$> valueChunks <*> (facts current)
        -- functorial voodoo:
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
