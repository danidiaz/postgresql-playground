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

main :: IO ()
main = do
  Right conn <- acquire ""
  let printResults :: forall x. Show x => Statement () [x] -> IO ()
      printResults q =
        do
          r <- q & statement () & flip run conn
          print r
  each actorSchema & limit 1 & select & printResults
  each addressSchema & limit 1 & select & printResults
  each categorySchema & limit 1 & select & printResults
  each citySchema & limit 1 & select & printResults
  each countrySchema & limit 1 & select & printResults
  each customerSchema & limit 1 & select & printResults
  each filmSchema & limit 1 & select & printResults
  each filmActorSchema & limit 1 & select & printResults
  each filmCategorySchema & limit 1 & select & printResults
  each inventorySchema & limit 1 & select & printResults
  each languageSchema & limit 1 & select & printResults
  each paymentSchema & limit 1 & select & printResults
  each rentalSchema & limit 1 & select & printResults
  each staffSchema & limit 1 & select & printResults
  each storeSchema & limit 1 & select & printResults
  paymentsByCustomer & limit 1 & select & printResults
  paymentsByCustomerAndStaff & limit 1 & select & printResults
  release conn
