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
    selectAllActors,

    -- * The rest
    module PagilaEsqueleto,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (NoLoggingT (..))
import Control.Monad.Trans.Reader
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
|]
 )

selectAllActors :: (MonadIO m) => ReaderT SqlBackend m [Entity Actor]
selectAllActors = select do
  actor <- from $ table @Actor
  limit 2
  pure actor

run :: ReaderT SqlBackend (NoLoggingT IO) r -> IO r
run q =
  runNoLoggingT $
    withPostgresqlConn "" \backend -> do
      runSqlConn q backend
