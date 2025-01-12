{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | https://hachyderm.io/@DiazCarrete/113810714496179726
-- https://dev.to/zelenya/how-to-use-postgresql-with-haskell-persistent-esqueleto-4n6i
module PagilaEsqueleto
  ( -- * Helpers
    run,

    -- * Queries
    selectSomeActors,
    selectSomeAddresses,
    selectSomeCategories,
    selectSomeCities,
    selectSomeCountries,
    selectSomeCustomers,
    selectSomeFilms,
    selectSomeFilmActors,
    selectSomeFilmCategories,
    selectSomeInventories,
    selectSomeLanguages,
    selectSomePayments,
    selectSomeRentals,
    selectSomeStores,
    selectSomeStaffs,

    -- * The rest
    module PagilaEsqueleto,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (NoLoggingT (..))
import Control.Monad.Trans.Reader
import Data.ByteString
import Data.Function ((&))
import Data.Int
import Data.Text
import Data.Time
import Database.Esqueleto.Experimental
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

$( share
     [mkPersist sqlSettings]
     [persistLowerCase|
        Actor sql=actor
            Id Int64 sql=actor_id
            firstName Text sql=first_name
            lastName Text sql=last_name
            lastUpdate UTCTime sql=last_update
            deriving Show
        Address sql=address
            Id Int64 sql=address_id
            address Text sql=address
            address2 Text Maybe sql=address2
            district Text sql=district
            cityId Int64 sql=city_id
            postalCode Text Maybe sql=postal_code
            phone Text sql=phone
            lastUpdate UTCTime sql=last_update
            deriving Show
        Category sql=category
            Id Int64 sql=category_id
            name Text sql=name
            lastUpdate UTCTime sql=last_update
            deriving Show          
        City sql=city
            Id Int64 sql=city_id
            city Text sql=city
            countryId Int64 sql=country_id
            lastUpdate UTCTime sql=last_update
            deriving Show
        Country sql=country
            Id Int64 sql=country_id
            country Text sql=country
            lastUpdate UTCTime sql=last_update
            deriving Show
        Customer sql=customer
            Id Int64 sql=customer_id
            storeId Int64 sql=store_id
            firstName Text sql=first_name
            lastName Text sql=last_name
            email Text Maybe sql=email
            addressId Int64 sql=address_id
            activeBool Bool sql=activebool
            createDate Day sql=create_date
            lastUpdate UTCTime Maybe sql=last_update
            active Int64 Maybe sql=active
            deriving Show
        Film sql=film
            Id Int64 sql=film_id
            title Text sql=title
            description Text Maybe sql=description
            languageId Int64 sql=language_id
            originalLanguageId Int64 Maybe sql=original_language_id
            rentalDuration Int64 sql=rental_duration
            rentalRate Double sql=rental_rate
            length Int64 Maybe sql=length
            replacementCost Double sql=replacement_cost
            lastUpdate UTCTime sql=last_update
            deriving Show
        FilmActor sql=film_actor
            actorId Int64 sql=actor_id
            filmId Int64 sql=film_id
            lastUpdate UTCTime sql=last_update
            Primary actorId filmId
            deriving Show
        FilmCategory sql=film_category
            filmId Int64 sql=film_id
            categoryId Int64 sql=category_id
            lastUpdate UTCTime sql=last_update
            Primary filmId categoryId
            deriving Show
        Inventory sql=inventory
            Id Int64 sql=inventory_id
            filmId Int64 sql=film_id
            storeId Int64 sql=store_id
            lastUpdate UTCTime sql=last_update
            deriving Show
        Language sql=language
            Id Int64 sql=language_id
            name Text sql=name
            lastUpdate UTCTime sql=last_update
            deriving Show
        Payment sql=payment
            Id Int64 sql=payment_id
            customerId Int64 sql=customer_id
            staffId Int64 sql=staff_id
            rentalId Int64 sql=rental_id
            amount Double sql=amount
            paymentDate UTCTime sql=payment_date
            deriving Show
        Rental sql=rental
            Id Int64 sql=rental_id
            rentalDate UTCTime sql=rental_date
            inventoryId Int64 sql=inventory_id
            customerId Int64 sql=customer_id
            returnDate UTCTime Maybe sql=return_date
            staffId Int64 sql=staff_id
            lastUpdate UTCTime sql=last_update
            deriving Show
        Store sql=store
            Id Int64 sql=store_id
            managerStaffId Int64 sql=manager_staff_id
            addressId Int64 sql=address_id
            lastUpdate UTCTime sql=last_update
            deriving Show
        Staff sql=staff
            Id Int64 sql=staff_id
            firstName Text sql=first_name
            lastName Text sql=last_name
            addressId Int64 sql=address_id
            email Text Maybe sql=email
            storeId Int64 sql=store_id
            active Bool sql=active
            username Text sql=username
            password Text Maybe sql=password
            lastUpdate UTCTime sql=last_update
            picture ByteString Maybe sql=picture
            deriving Show
|]
 )

selectSomeActors :: (MonadIO m) => ReaderT SqlBackend m [Entity Actor]
selectSomeActors = select do
  actor <- from $ table @Actor
  limit 2
  pure actor

selectSomeAddresses :: (MonadIO m) => ReaderT SqlBackend m [Entity Address]
selectSomeAddresses = select do
  actor <- from $ table @Address
  limit 2
  pure actor

selectSomeCategories :: (MonadIO m) => ReaderT SqlBackend m [Entity Category]
selectSomeCategories = select do
  actor <- from $ table @Category
  limit 2
  pure actor

selectSomeCities :: (MonadIO m) => ReaderT SqlBackend m [Entity City]
selectSomeCities = select do
  actor <- from $ table @City
  limit 2
  pure actor

selectSomeCountries :: (MonadIO m) => ReaderT SqlBackend m [Entity Country]
selectSomeCountries = select do
  country <- from $ table @Country
  limit 2
  pure country

selectSomeCustomers :: (MonadIO m) => ReaderT SqlBackend m [Entity Customer]
selectSomeCustomers = select do
  customer <- from $ table @Customer
  limit 2
  pure customer

selectSomeFilms :: (MonadIO m) => ReaderT SqlBackend m [Entity Film]
selectSomeFilms = select do
  film <- from $ table @Film
  limit 2
  pure film

selectSomeFilmActors :: (MonadIO m) => ReaderT SqlBackend m [Entity FilmActor]
selectSomeFilmActors = select do
  filmActor <- from $ table @FilmActor
  limit 2
  pure filmActor

selectSomeFilmCategories :: (MonadIO m) => ReaderT SqlBackend m [Entity FilmCategory]
selectSomeFilmCategories = select do
  filmCategory <- from $ table @FilmCategory
  limit 2
  pure filmCategory

selectSomeInventories :: (MonadIO m) => ReaderT SqlBackend m [Entity Inventory]
selectSomeInventories = select do
  inventory <- from $ table @Inventory
  limit 2
  pure inventory

selectSomeLanguages :: (MonadIO m) => ReaderT SqlBackend m [Entity Language]
selectSomeLanguages = select do
  language <- from $ table @Language
  limit 2
  pure language

selectSomePayments :: (MonadIO m) => ReaderT SqlBackend m [Entity Payment]
selectSomePayments = select do
  payment <- from $ table @Payment
  limit 2
  pure payment

selectSomeRentals :: (MonadIO m) => ReaderT SqlBackend m [Entity Rental]
selectSomeRentals = select do
  rental <- from $ table @Rental
  limit 2
  pure rental

selectSomeStores :: (MonadIO m) => ReaderT SqlBackend m [Entity Store]
selectSomeStores = select do
  store <- from $ table @Store
  limit 2
  pure store

selectSomeStaffs :: (MonadIO m) => ReaderT SqlBackend m [Entity Staff]
selectSomeStaffs = select do
  staff <- from $ table @Staff
  limit 2
  pure staff

run :: ReaderT SqlBackend (NoLoggingT IO) r -> IO r
run q =
  runNoLoggingT $
    withPostgresqlConn "" \backend -> do
      runSqlConn q backend
