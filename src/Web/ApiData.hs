{-# LANGUAGE FlexibleInstances #-}

-- This module provides additions to Domain types required for API parsing
-- I wanted to keep them in a separate module, so the original Domain module
-- remains free of Servant-specific code.
-- Instances should not be orphans, hence we had to go with the newtype approach.
-- A newtype PathId is used for that purpose.

module Web.ApiData
  ( PathClapId,
    PathUserId,
    extractClapId,
    extractUserId,
  )
where

import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Domain.Clap (ClapId (..))
import Domain.User (UserId (..))
import Servant (FromHttpApiData (..))

-- Generic newtype for HTTP parsing
newtype PathId a = PathId {unPathId :: a}

-- fromPathId :: PathId a -> a
-- fromPathId (PathId _id) = _id

instance (UUIDable a) => FromHttpApiData (PathId a) where
  parseUrlPiece piece = case UUID.fromText piece of
    Nothing -> Left . T.pack $ "Invalid UUID-piece " ++ show piece
    Just uuid -> return $ PathId (fromUUID uuid)

class UUIDable a where
  fromUUID :: UUID -> a

instance UUIDable ClapId where
  fromUUID = ClapId

instance UUIDable UserId where
  fromUUID = UserId

-- Type synonyms for specific PathId types
-- that are used in the API definitions

type PathClapId = PathId ClapId

type PathUserId = PathId UserId

-- Function to extract ClapId from PathId
extractClapId :: PathId ClapId -> ClapId
extractClapId = unPathId

-- Function to extract UserId from PathId
extractUserId :: PathId UserId -> UserId
extractUserId = unPathId
