{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module : Database
-- Copyright : (C) 2015 Braden Walters
-- License : MIT (see LICENSE file)
-- Maintainer : (C) Braden Walters <vc@braden-walters.info>
-- Stability : experimental
--
-- This is where interaction with the acid-state database occurs.
module Database
( Database(..)
, dbIncDatabaseTest
, dbGetDatabaseTest
) where

import Data.Monoid
import Data.SafeCopy
import Data.Typeable
import Control.Lens
import Control.Monad.Reader
import Snap.Snaplet.AcidState
import Control.Monad.State

data Database = Database { _testDb :: Integer }
  deriving (Show, Ord, Eq, Typeable)

deriveSafeCopy 0 'base ''Database

makeLenses ''Database

instance Monoid Database where
  mempty = Database 0

incDatabaseTest :: Update Database ()
incDatabaseTest = modify $ over testDb (+ 1)

getDatabaseTest :: Query Database Integer
getDatabaseTest = asks _testDb

makeAcidic ''Database ['incDatabaseTest, 'getDatabaseTest]

dbIncDatabaseTest = IncDatabaseTest
dbGetDatabaseTest = GetDatabaseTest
