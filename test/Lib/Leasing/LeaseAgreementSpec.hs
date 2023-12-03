module Lib.Leasing.LeaseAgreementSpec (spec) where

import qualified Lib.Leasing.LeasableAsset as LA
import Lib.Leasing.LeaseAgreement
import Test.Hspec

primaryAsset :: LA.LeasableAsset
primaryAsset =
  LA.LeasableAsset
    { LA.id = "82f2b54b-c41b-4650-bd9b-346408c1a95a",
      LA.kind = LA.Vehicle,
      LA.condition = LA.New
    }

primaryLesseeId :: String
primaryLesseeId = "42f2b54b-c41b-4650-bd9b-346408c1a95b"

primaryLeaseAgreement :: LeaseAgreement
primaryLeaseAgreement =
  LeaseAgreement
    { agreementId = "72f2b54b-c41b-4650-bd9b-346408c1a95a",
      asset = primaryAsset,
      lesseeId = primaryLesseeId,
      leaseTerm = (read "2022-01-01", read "2023-01-01"),
      monthlyRate = (USD, 1000.0)
    }

spec :: Spec
spec = do
  describe "durationMonths" $ do
    it "returns the correct duration in months" $ do
      durationMonths primaryLeaseAgreement `shouldBe` 12

  describe "totalCost" $ do
    it "returns the correct total cost" $ do
      totalCost primaryLeaseAgreement `shouldBe` 12000.0

  describe "assetsLeasedBy" $ do
    it "returns the assets leased by a specific lessee" $
      do
        [primaryLeaseAgreement] `assetsLeasedBy` primaryLesseeId
        `shouldBe` [primaryAsset]

  describe "averageLeaseCostByAssetKind" $ do
    it "returns the average lease cost for a specific asset kind" $ do
      let agreements =
            [ primaryLeaseAgreement,
              primaryLeaseAgreement
                { monthlyRate = (USD, 2000.0)
                }
            ]

      averageLeaseCostByAssetKind LA.Vehicle agreements `shouldBe` Just 18000.0

  describe "agreementsByAssetKind" $ do
    it "returns the agreements for a specific asset kind" $ do
      let agreements =
            [ primaryLeaseAgreement,
              primaryLeaseAgreement
                { asset =
                    primaryAsset
                      { LA.kind = LA.RealEstate
                      }
                },
              primaryLeaseAgreement
                { asset =
                    primaryAsset
                      { LA.kind = LA.Equipment
                      }
                },
              primaryLeaseAgreement,
              primaryLeaseAgreement
            ]
      length (agreementsByAssetKind agreements LA.Vehicle) `shouldBe` 3
