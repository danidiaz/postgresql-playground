{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

-- import Control.Lens

import Data.Function ((&))
import Data.Generics.Product.Fields (field)
import Data.Int
import Data.Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hasql.Connection (acquire, release)
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement)
import Rel8
import Prelude
import Data.ByteString (ByteString)

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
    { name = "customer",
      schema = Nothing,
      columns =
        Customer
          { customerId = "customer_id",
            storeId = "store_id",
            firstName = "first_name",
            lastName = "last_name",
            email = "email",
            addressId = "address_id",
            activeBool = "activebool",
            createDate = "create_date",
            lastUpdate = "last_update",
            active = "active"
          }
    }

-- | table film
newtype FilmId = FilmId {bareFilmId :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show)

data Film f = Film
  { filmId :: Column f FilmId,
    title :: Column f Text,
    description :: Column f (Maybe Text),
    -- releaseYear :: Column f (Maybe UTCTime), -- which is the correct type here?
    languageId :: Column f LanguageId,
    originalLanguageId :: Column f (Maybe LanguageId),
    rentalDuration :: Column f Int64,
    rentalRate :: Column f Float,
    length :: Column f (Maybe Int64),
    replacementCost :: Column f Float,
    -- rating :: Column f (Maybe UTCTime), -- which is the correct type here?
    lastUpdate :: Column f UTCTime
    -- specialFeatures :: Column f (Maybe UTCTime), -- which is the correct type here?
    -- fullText :: Column f UTCTime, -- which is the correct type here?
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Film f)

filmSchema :: TableSchema (Film Name)
filmSchema =
  TableSchema
    { name = "film",
      schema = Nothing,
      columns =
        Film
          { filmId = "film_id",
            title = "title",
            description = "description",
            -- releaseYear = "release_year"
            languageId = "language_id",
            originalLanguageId = "original_language_id",
            rentalDuration = "rental_duration",
            rentalRate = "rental_rate",
            length = "length",
            replacementCost = "replacement_cost",
            -- rating = "rating",
            lastUpdate = "last_update"
            -- specialFeatures = "special_features"
            -- fullText = "fulltext"
          }
    }

-- | table film_actor
data FilmActor f = FilmActor
  { actorId :: Column f ActorId,
    filmId :: Column f FilmId,
    lastUpdate :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (FilmActor f)

filmActorSchema :: TableSchema (FilmActor Name)
filmActorSchema =
  TableSchema
    { name = "film_actor",
      schema = Nothing,
      columns =
        FilmActor
          { actorId = "actor_id",
            filmId = "film_id",
            lastUpdate = "last_update"
          }
    }

-- | table film_category
data FilmCategory f = FilmCategory
  { 
    filmId :: Column f FilmId,
    categoryId :: Column f CategoryId,
    lastUpdate :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (FilmCategory f)

filmCategorySchema :: TableSchema (FilmCategory Name)
filmCategorySchema =
  TableSchema
    { name = "film_category",
      schema = Nothing,
      columns =
        FilmCategory
          { 
            filmId = "film_id",
            categoryId = "category_id",
            lastUpdate = "last_update"
          }
    }

-- | table inventory
newtype InventoryId = InventoryId {bareInventoryId :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show)

data Inventory f = Inventory
  { 
    inventoryId :: Column f FilmId,
    filmId :: Column f FilmId,
    storeId :: Column f StoreId,
    lastUpdate :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Inventory f)

inventorySchema :: TableSchema (Inventory Name)
inventorySchema =
  TableSchema
    { name = "inventory",
      schema = Nothing,
      columns =
        Inventory
          { 
            inventoryId = "film_id",
            filmId = "film_id",
            storeId = "store_id",
            lastUpdate = "last_update"
          }
    }

-- | table language
newtype LanguageId = LanguageId {bareLanguageId :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show)

data Language f = Language
  { languageId :: Column f LanguageId,
    name :: Column f Text,
    lastUpdate :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Language f)

languageSchema :: TableSchema (Language Name)
languageSchema =
  TableSchema
    { name = "language",
      schema = Nothing,
      columns =
        Language
          { languageId = "language_id",
            name = "name",
            lastUpdate = "last_update"
          }
    }

-- | table payment

newtype PaymentId = PaymentId {barePaymentId :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show)

data Payment f = Payment
  { paymentId :: Column f PaymentId,
    customerId :: Column f CustomerId,
    staffId :: Column f StaffId,
    rentalId :: Column f RentalId,
    amount :: Column f Float,
    paymentDate :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Payment f)

paymentSchema :: TableSchema (Payment Name)
paymentSchema =
  TableSchema
    { name = "payment",
      schema = Nothing,
      columns =
        Payment
          { paymentId = "payment_id",
            customerId = "customer_id",
            staffId = "staff_id",
            rentalId = "rental_id",
            amount = "amount",
            paymentDate = "payment_date"
          }
    }

-- | table rental
newtype RentalId = RentalId {bareRentalId :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show)

data Rental f = Rental
  { rentalId :: Column f RentalId,
    rentalDate :: Column f UTCTime,
    inventoryId :: Column f InventoryId,
    customerId :: Column f CustomerId,
    returnDate :: Column f (Maybe UTCTime),
    staffId :: Column f StaffId,
    lastUpdate :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Rental f)

rentalSchema :: TableSchema (Rental Name)
rentalSchema =
  TableSchema
    { name = "rental",
      schema = Nothing,
      columns =
        Rental
          { rentalId = "rental_id",
            rentalDate = "rental_date",
            inventoryId = "inventory_id",
            customerId = "customer_id",
            returnDate = "return_date",
            staffId = "staff_id",
            lastUpdate = "last_update"
          }
    }

-- | table store
newtype StoreId = StoreId {bareStoreId :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show)

data Store f = Store
  { storeId :: Column f StoreId,
    managerStaffId :: Column f StaffId,
    addressId :: Column f AddressId,
    lastUpdate :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Store f)

storeSchema :: TableSchema (Store Name)
storeSchema =
  TableSchema
    { name = "store",
      schema = Nothing,
      columns =
        Store
          { storeId = "store_id",
            managerStaffId = "manager_staff_id",
            addressId = "address_id",
            lastUpdate = "last_update"
          }
    }

-- | table staff
newtype StaffId = StaffId {bareStaffId :: Int64}
  deriving newtype (DBEq, DBType, Eq, Show)

data Staff f = Staff
  { staffId :: Column f StaffId,
    firstName :: Column f Text,
    lastName :: Column f Text,
    addressId :: Column f AddressId,
    email :: Column f (Maybe Text),
    storeId :: Column f StoreId,
    active ::  Column f Bool,
    username ::  Column f Text,
    password ::  Column f (Maybe Text),
    lastUpdate :: Column f UTCTime,
    picture :: Column f ByteString
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Staff f)

staffSchema :: TableSchema (Staff Name)
staffSchema =
  TableSchema
    { name = "staff",
      schema = Nothing,
      columns =
        Staff
          { staffId = "store_id",
            firstName = "first_name",
            lastName = "last_name",
            addressId = "address_id",
            email = "email",
            storeId = "store_id",
            active = "active",
            username = "username",
            password = "password",
            lastUpdate = "last_update",
            picture = "picture"
          }
    }

-- | aggregation
paymentsByCustomer :: Query (Expr CustomerId, Expr Float)
paymentsByCustomer = aggregate do
  payment <- each paymentSchema
  let customerId = Rel8.groupBy $ payment.customerId
  let sumAmount = Rel8.sum $ payment.amount
  pure (customerId,sumAmount)

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
  release conn
