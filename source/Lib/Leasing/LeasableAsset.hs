module Lib.Leasing.LeasableAsset
  ( LeasableAsset (..),
    AssetCondition (..),
    AssetKind (..),
    filterByCondition,
    isInCondition,
    isOfKind,
  )
where

data AssetCondition = New | Good | Fair | Poor deriving (Show, Eq)

data AssetKind = Vehicle | RealEstate | Equipment deriving (Show, Eq)

data LeasableAsset = LeasableAsset
  { id :: String,
    kind :: AssetKind,
    condition :: AssetCondition
  }
  deriving (Show, Eq)

filterByCondition :: AssetCondition -> [LeasableAsset] -> [LeasableAsset]
filterByCondition cond = filter (`isInCondition` cond)

isInCondition :: LeasableAsset -> AssetCondition -> Bool
asset `isInCondition` cond = condition asset == cond

isOfKind :: LeasableAsset -> AssetKind -> Bool
asset `isOfKind` assetKind = kind asset == assetKind