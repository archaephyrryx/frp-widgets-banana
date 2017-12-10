{-# LANGUAGE MultiParamTypeClasses #-}
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
import Util hiding (visible, Visible)
import Prelude hiding ((!!))
import Util.List.Safe ((!!))

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

instance Visible Cast where
  visible = castAttr (_table._elem._cast) visible
  refresh = refresh . _table . _elem . _cast

-- | Given a list length and a default value, transform a @Behavior [a]@ to a $[Behavior a]$, with
-- any out-of-range elements being the constant behavior containing the default value
blTranspose :: Int -> a -> Behavior [a] -> [Behavior a]
blTranspose n z bxs = map (\x -> (!!x) <$> (take n . (++(repeat z)) <$> bxs)) (enumFromTo 0 (n-1))

-- | Transform lists of Behaviors into Behaviors of lists
lbTranspose :: [Behavior a] -> Behavior [a]
lbTranspose = foldr (\x acc -> (:) <$> x <*> acc) (pure [])


-- | Function from Cast element to the link-label
type Label a = StaticDynamic (a -> String)

-- | Function from Cast element and Link to full Cast row
type Wrapper a = StaticDynamic (a -> Link Int -> Row)

-- | Transformer from flat row-list to final row-list (enables gridding for Obscurae)
type Collector = StaticDynamic ([Row] -> [Row])

-- | Definition of the elemental layout of a cast, determining how each item is ultimately displayed
data Format a = Format { label :: Label a, wrap :: Wrapper a, collect :: Collector }

-- | Definition of the functional content of a cast, determining the items overall and which are in
-- focus at any given time, as well as the format definition
data CastType a = Cask { bContents :: Behavior [a]
                       , pagesize  :: Int
                       , current   :: Tidings Int
                       , format    :: Format a
                       }
                | Case { contents  :: [a]
                       , bPagesize :: Behavior Int
                       , current   :: Tidings Int
                       , format    :: Format a
                       }


-- | Returns the index of the final page of a Cast
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

synthesize :: CastType a -> Panel () ->  MomentIO ([Link Int])
synthesize x@(Cask{..}) _tab = do
  let
      -- Reactive list of indices on current page
      bCurrentPage :: Behavior [Int]
      bCurrentPage = (!!) <$> bIndexChunks x <*> facts current

      -- List of reactive indices of current page
      indexBs :: [Behavior Int]
      indexBs = blTranspose pagesize (-1) bCurrentPage

      -- Create link from button and reactive index
      generate :: Behavior Int -> LButton -> MomentIO (Link Int)
      generate b l = liquidLink l (flux . sdIndexLabel $ x) b

      -- Convert the reactive index in a static position to its liquidlink
      produce :: Behavior Int -> MomentIO (Link Int)
      produce b = (liftIO $ preLink _tab) >>= generate b

  mapM produce indexBs


synthesize x@(Case{..}) _tab = do
  let
      indices :: [Int]
      indices = indicate contents

      label :: Int -> String -- Determines text of nth SoftLink
      label = freeze . sdIndexLabel $ x


      extrude :: Int -> MomentIO (Link Int)
      extrude y = do { l <- liftIO $ preLink _tab;
                       -- sink l [ visible :== (!!y) <$> bShow ];
                       softLink l label y;
                     }

  mapM extrude indices




-- | Generate a generic Cast from a CastType and the table frame it will occupy
genCast :: CastType a -> Table -> MomentIO Cast
genCast x@(Cask{..}) tab@(Table _tab) = do
    liquids <- synthesize x _tab
    let
        valueChunks = bValueChunks x
        -- current range of values
        bValues = (!!) <$> valueChunks <*> (facts current)
        -- functorial voodoo:
        bLinks :: Behavior [Link Int]
        bLinks = (`filtrate`liquids) <$> (lbTranspose $ map (((>=0) <$>).(flux.rubicon)) liquids)
        rawRows :: Behavior [Row]
        rawRows = zipWith <$> flux (wrap format) <*> bValues <*> bLinks
    liquidBox <- tabulate tab $ combine (collect format) rawRows

    let _cast = ECast liquidBox
        _actuate = tidings (pure (-1)) $ priorityUnion (((-1) <$ rumors current):map portents liquids)
    return Cast{..}
genCast x@(Case{..}) tab@(Table _tab) = do --mdo
    softs <- synthesize x _tab

    let

        bCur :: Behavior Int
        bCur = facts current

        bPageNumbers :: Behavior [[Int]] -- Lists of indexes for each page
        bPageNumbers = (`chunksOf`(indicate contents)) <$> bPagesize
        bShows :: Behavior ([Row] -> [Bool]) -- Indicator function for paged-in rows
        bShows = verity <$> ((!!) <$> bPageNumbers <*> bCur)

        eSofts :: [Event Int]
        eSofts = map blood softs
        bChunks :: Behavior [[Link Int]]
        bChunks = (`chunksOf`softs) <$> bPagesize
        allRows :: Behavior [Row]
        allRows = (\x -> zipWith x contents softs) <$> (flux . wrap $ format)
        bShow :: Behavior [Bool]
        bShow = bShows <*> allRows
        bSofts :: Behavior [Link Int]
        bSofts = (!!) <$> bChunks <*> bCur

        curValues = (!!) <$> bValueChunks x <*> bCur
        bRows :: Behavior [Row]
        bRows = zipWith <$> (flux . wrap $ format) <*> curValues <*> bSofts

    flip mapM softs (\SoftLink{..} -> sink _link [ visible :== (!!(_crux)) <$> bShow ])

    softBox <- tabulate tab $ combine (collect format) bRows
    redrawRows bShows allRows


    let _cast = ECast softBox
        _actuate = tidings (pure (-1)) $ priorityUnion (((-1) <$ rumors current):eSofts)
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
