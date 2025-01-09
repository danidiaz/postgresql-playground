{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main where

import PagilaRel8
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Int
import Data.Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hasql.Connection (acquire, release)
import Hasql.Session qualified
import Hasql.Statement (Statement)
import Hasql.Statement qualified
import Rel8 (select, each, limit, run)
import Rel8 qualified


main :: IO ()
main = do
  Right conn <- acquire ""
  let printResults :: forall x. Show x => Statement () [x] -> IO ()
      printResults q =
        do
          r <- q & Hasql.Session.statement () & flip Hasql.Session.run conn
          print r
  each actorSchema & limit 1 & select & run & printResults
  each addressSchema & limit 1 & select & run & printResults
  each categorySchema & limit 1 & select & run & printResults
  each citySchema & limit 1 & select & run & printResults
  each countrySchema & limit 1 & select & run & printResults
  each customerSchema & limit 1 & select & run & printResults
  each filmSchema & limit 1 & select & run & printResults
  each filmActorSchema & limit 1 & select & run & printResults
  each filmCategorySchema & limit 1 & select & run & printResults
  each inventorySchema & limit 1 & select & run & printResults
  each languageSchema & limit 1 & select & run & printResults
  each paymentSchema & limit 1 & select & run & printResults
  each rentalSchema & limit 1 & select & run & printResults
  each staffSchema & limit 1 & select & run & printResults
  each storeSchema & limit 1 & select & run & printResults
  paymentsByCustomer & limit 1 & select & run & printResults
  paymentsByCustomerAndStaff & limit 1 & select & run & printResults
  release conn
