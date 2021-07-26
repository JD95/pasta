module Runtime.Dsl where

import qualified Data.Vector as Vec
import Numeric.Natural
import Runtime.Types

rtSeq :: [RtCtl] -> RtCtl
rtSeq [] = error "rtSeq applied to empty list"
rtSeq ps = foldr1 RtEval ps

rtCase :: Natural -> [(Match, RtCtl)] -> RtCtl
rtCase x = RtEval (RtStackVar x) . RtBranch (RtStackVar x) . Vec.fromList

rtInt :: Int -> RtCtl
rtInt = RtAllocPrim . RtInt

rtProd :: [RtCtl] -> RtCtl
rtProd = RtAllocProd . Vec.fromList

rtIndex :: RtCtl -> Natural -> RtCtl
rtIndex = RtIndex

rtCon :: Natural -> RtCtl -> RtCtl
rtCon = RtAllocCon

rtApp :: RtCtl -> [RtCtl] -> RtCtl
rtApp x = RtApp x . Vec.fromList

rtThunk :: [RtCtl] -> RtCtl -> RtCtl
rtThunk xs x = RtAllocThunk x $ Vec.fromList xs

rtProp ::
  -- | The target cell
  RtCtl ->
  -- | Sources
  [RtCtl] ->
  -- | First stack var will be the target cell
  -- the rest will be the sources
  RtCtl ->
  RtCtl
rtProp target srcs = RtAllocProp target (fromIntegral $ length srcs - 1) (Vec.fromList srcs)

rtCellDeps :: [RtCtl] -> RtCtl -> RtCtl
rtCellDeps [] cell = cell
rtCellDeps ps cell = foldr1 RtEval $ RtAddCellDep cell <$> ps

rtCell :: RtCtl -> RtCtl
rtCell = RtAllocCell

rtInformCell :: RtCtl -> RtCtl -> RtCtl
rtInformCell = RtInformCell

rtInfoEmpty :: RtCtl
rtInfoEmpty = RtAllocCon 0 unit

rtInfoPartial :: RtCtl -> RtCtl
rtInfoPartial = RtAllocCon 1

rtInfoTop :: RtCtl -> RtCtl
rtInfoTop = RtAllocCon 2

rtInfoConflict :: RtCtl
rtInfoConflict = RtAllocCon 3 unit

rtBox :: RtCtl -> RtClo r
rtBox x = RtThunk x (Vec.fromList [])

rtVar :: Natural -> RtCtl
rtVar = RtStackVar

rtFree :: Natural -> RtCtl
rtFree = RtFreeVar

identityLat :: RtCtl
identityLat =
  rtCase
    0 -- Match on the existing cell info
    [ -- If it's empty, fill with the
      -- new value and mark as Top
      (MCon 0, rtInfoTop $ rtVar 1),
      -- If there is any information
      -- already in the cell, raise a
      -- conflict
      (MAny, rtInfoConflict)
    ]

-- | Propagation that simply passes the value to the cell
idProp :: RtCtl
idProp = rtInformCell (rtVar 0) (rtVar 1)

unit :: RtCtl
unit = RtAllocProd $ Vec.fromList []

true :: RtCtl
true = RtAllocCon 0 unit

false :: RtCtl
false = RtAllocCon 1 unit
