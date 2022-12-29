{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

-- import Control.Lens

import Data.Function ((&))
import Data.Generics.Product.Fields (field)
import Data.Int
import Data.Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hasql.Connection (acquire, release)
import Hasql.Session (run, statement, QueryError)
import Rel8
import Prelude
import Hasql.Statement (Statement)

-- | table actor
newtype ActorId = ActorId {bareActorId :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show)

data Actor f = Actor
  { -- We prefix the pk with the datatype name, not to avoid collisions (we are using DuplicateRecordFields after all),
    -- but to follow this convention:
    -- https://dba.stackexchange.com/questions/16616/why-do-people-recommend-not-using-the-name-id-for-an-identity-column
    -- https://softwareengineering.stackexchange.com/a/114730/76774
    actorId :: Column f ActorId,
    firstName :: Column f Text,
    lastName :: Column f Text,
    lastUpdate :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Actor f)

actorSchema :: TableSchema (Actor Name)
actorSchema =
  TableSchema
    { name = "actor",
      schema = Nothing,
      columns =
        Actor
          { actorId = "actor_id",
            firstName = "first_name",
            lastName = "last_name",
            lastUpdate = "last_update"
          }
    }

-- | table address
newtype AddressId = AddressId {bareAddressId :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show)

data Address f = Address
  { addressId :: Column f AddressId,
    address :: Column f Text,
    address2 :: Column f (Maybe Text), -- nullable
    district :: Column f Text,
    cityId :: Column f CityId,
    postalCode :: Column f (Maybe Text),
    phone :: Column f Text,
    lastUpdate :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Address f)

addressSchema :: TableSchema (Address Name)
addressSchema =
  TableSchema
    { name = "address",
      schema = Nothing,
      columns =
        Address
          { addressId = "address_id",
            address = "address",
            address2 = "address2", -- nullable
            district = "district",
            cityId = "city_id",
            postalCode = "postal_code",
            phone = "phone",
            lastUpdate = "last_update"
          }
    }


-- | table category
newtype CategoryId = CategoryId {bareCategoryId :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show)

data Category f = Category
  { categoryId :: Column f CategoryId,
    name :: Column f Text,
    lastUpdate :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Category f)

categorySchema :: TableSchema (Category Name)
categorySchema =
  TableSchema
    { name = "category",
      schema = Nothing,
      columns =
        Category
          { categoryId = "category_id",
            name = "name",
            lastUpdate = "last_update"
          }
    }

-- | table city
newtype CityId = CityId {bareCityId :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show)

data City f = City
  { cityId :: Column f CityId,
    city :: Column f Text,
    countryId :: Column f CountryId,
    lastUpdate :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (City f)

citySchema :: TableSchema (City Name)
citySchema =
  TableSchema
    { name = "city",
      schema = Nothing,
      columns =
        City
          { cityId = "city_id",
            city = "city",
            countryId = "country_id",
            lastUpdate = "last_update"
          }
    }

-- | table country
newtype CountryId = CountryId {bareCountryId :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show)

data Country f = Country
  { countryId :: Column f CityId,
    country :: Column f Text,
    lastUpdate :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Country f)

countrySchema :: TableSchema (Country Name)
countrySchema =
  TableSchema
    { name = "country",
      schema = Nothing,
      columns =
        Country
          { countryId = "country_id",
            country = "country",
            lastUpdate = "last_update"
          }
    }

-- | table customer
newtype CustomerId = CustomerId {bareCustomerId :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show)

newtype StoreId = StoreId {bareStoreId :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show)

data Customer f = Customer
  { customerId :: Column f CityId,
    storeId :: Column f StoreId,
    firstName :: Column f Text,
    lastName :: Column f Text,
    email :: Column f (Maybe Text),
    addressId :: Column f AddressId,
    activeBool :: Column f Bool,
    createDate :: Column f UTCTime,
    lastUpdate :: Column f (Maybe UTCTime),
    active :: Column f (Maybe Int64)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Customer f)

customerSchema :: TableSchema (Customer Name)
customerSchema =
  TableSchema
  { 
     name = "customer",
      schema = Nothing,
      columns = Customer {
    
    customerId = "customer_id" ,
    storeId = "store_id",
    firstName = "first_name",
    lastName  = "last_name",
    email = "email",
    addressId = "address_id",
    activeBool = "activebool",
    createDate = "create_date",
    lastUpdate = "last_update",
    active = "active"
  }}



main :: IO ()
main = do
  Right conn <- acquire ""
  let printResults :: forall x . Show x => Statement () [x] -> IO ()
      printResults q = 
        do r <- q & statement () & flip run conn 
           print r
  each actorSchema & limit 1 & select & printResults
  each addressSchema & limit 1 & select & printResults
  each categorySchema & limit 1 & select & printResults
  each citySchema & limit 1 & select & printResults
  each countrySchema & limit 1 & select & printResults
  each customerSchema & limit 1 & select & printResults
  release conn
    

