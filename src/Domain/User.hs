{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Domain.User
  ( User (..),
    UserName (..),
    UserEmail (..),
    UserId (..),
    UserCreate (..),
  )
where

import Data.Aeson
import Data.Aeson.TH
import Data.UUID (UUID)

newtype UserId = UserId {unUserId :: UUID}
  deriving (Eq, Show, ToJSON, FromJSON)

newtype UserName = UserName {unUserName :: String}
  deriving (Eq, Show, ToJSON, FromJSON)

newtype UserEmail = UserEmail {unUserEmail :: String}
  deriving (Eq, Show, ToJSON, FromJSON)

data User = User
  { userId :: UserId,
    name :: UserName,
    email :: UserEmail
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

data UserCreate = UserCreate
  { name :: UserName,
    email :: UserEmail
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''UserCreate)
