module Lib.Leasing.LeaseAgreement
  ( LeaseAgreement (..),
    Currency (..),
    Rate,
    DayRange,
    durationMonths,
    totalCost,
    assetsLeasedBy,
    averageLeaseCostByAssetKind,
    agreementsByAssetKind,
  )
where

import Data.Time.Calendar (Day)
import Lib.Data.Time.Calendar (monthsBetween)
import qualified Lib.Leasing.LeasableAsset as LA

data Currency = USD | EUR | UAH deriving (Show, Eq)

type Rate = (Currency, Float)

type DayRange = (Day, Day)

data LeaseAgreement = LeaseAgreement
  { agreementId :: String,
    asset :: LA.LeasableAsset,
    lesseeId :: String,
    leaseTerm :: DayRange,
    monthlyRate :: Rate
  }
  deriving (Show, Eq)

durationMonths :: LeaseAgreement -> Integer
durationMonths agreement =
  let (startDay, endDay) = leaseTerm agreement
   in monthsBetween startDay endDay

totalCost :: LeaseAgreement -> Float
totalCost agreement =
  let (_, rate) = monthlyRate agreement
   in fromIntegral (durationMonths agreement) * rate

assetsLeasedBy :: [LeaseAgreement] -> String -> [LA.LeasableAsset]
agreements `assetsLeasedBy` lesseeId' =
  map asset $ filter (lesseeId' `ownsAssetOf`) agreements

ownsAssetOf :: String -> LeaseAgreement -> Bool
lesseeId' `ownsAssetOf` agreement = lesseeId agreement == lesseeId'

averageLeaseCostByAssetKind :: LA.AssetKind -> [LeaseAgreement] -> Maybe Float
averageLeaseCostByAssetKind assetKind agreements
  | count == 0 = Nothing
  | otherwise = Just (sum totalCosts / fromIntegral count)
  where
    totalCosts = map totalCost (filter ((== assetKind) . LA.kind . asset) agreements)
    count = length totalCosts

agreementsByAssetKind :: [LeaseAgreement] -> LA.AssetKind -> [LeaseAgreement]
agreementsByAssetKind agreements assetKind =
  filter ((`LA.isOfKind` assetKind) . asset) agreements
