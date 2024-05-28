{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Domain.Clap
  ( Clap (..),
    ClapId (..),
    ClapCreate (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.UUID (UUID)
import Domain.User (UserId)

-- ClapId is a newtype wrapper around UUID
-- It derives FromJSON and ToJSON instances for automatic serialization
newtype ClapId = ClapId {unClapId :: UUID}
  deriving (Eq, Show, ToJSON, FromJSON)

-- Clap model
data Clap = Clap
  { clapId :: ClapId,
    count :: Int,
    userId :: UserId
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Clap)

-- ClapCreate model
data ClapCreate = ClapCreate
  { userId :: UserId,
    initCount :: Int
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''ClapCreate)
