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

import Prelude
import Rel8
import Data.Text
import GHC.Generics ( Generic )
-- import Control.Lens
import Data.Generics.Product.Fields (field)
import Data.Int
import Data.Function ((&))
import Hasql.Session (run, statement)
import Hasql.Connection (acquire)
import Data.Time (UTCTime)

newtype ActorId = ActorId { bareActorId :: Int64 }
  deriving newtype (DBEq, DBType, Eq, Show)

data Actor f = Actor
  { id   :: Column f ActorId
  , firstName :: Column f Text
  , lastName :: Column f Text
  , lastUpdate  :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance f ~ Result => Show (Actor f)

actorSchema :: TableSchema (Actor Name)
actorSchema = TableSchema
  { name = "actor"
  , schema = Nothing
  , columns = Actor
      { id = "actor_id"
      , firstName = "first_name"
      , lastName = "last_name"
      , lastUpdate = "last_update"
      }
  }

main :: IO ()
main = pure ()
