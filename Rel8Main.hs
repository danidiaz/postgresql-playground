{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Hasql.Connection (acquire)
import Hasql.Session (run, statement)
import Rel8
import Prelude

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

main :: IO ()
main = pure ()
