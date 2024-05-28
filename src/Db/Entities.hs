{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Db.Entities
  ( migrateAll,
    ClapEntity (..),
    UserEntity (..),
    generateUserEntityKey,
    generateClapEntityKey,
    entityToUser,
    userCreateToEntity,
    clapCreateToEntity,
    clapToEntity,
    entityToClap,
    unClapEntityKey,
    userIdToKey,
    clapIdToKey,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import Database.Persist
import Database.Persist.TH
import Domain.Clap
import Domain.User
import Util (toUUID)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
ClapEntity
    Id Text
    count Int
    userId UserEntityId
    deriving Show
UserEntity
    Id Text
    name Text
    email Text
    deriving Show
|]

-- KEY generators

-- Generic function to generate a UUID key
generateKey :: (MonadIO m) => (Text -> Key a) -> m (Key a)
generateKey constructor = do
  constructor . T.pack . show <$> liftIO UUIDv4.nextRandom

-- Specialized functions using the generic `generateKey`
generateUserEntityKey :: (MonadIO m) => m (Key UserEntity)
generateUserEntityKey = generateKey UserEntityKey

generateClapEntityKey :: (MonadIO m) => m (Key ClapEntity)
generateClapEntityKey = generateKey ClapEntityKey

-- ID mappers

userIdToKey :: UserId -> Key UserEntity
userIdToKey uid = UserEntityKey $ UUID.toText (unUserId uid)

clapIdToKey :: ClapId -> Key ClapEntity
clapIdToKey uid = ClapEntityKey $ UUID.toText (unClapId uid)

--- Entity/Domain MAPPERS

-- Convert UserEntity to User
entityToUser :: Entity UserEntity -> User
entityToUser (Entity uid userEntity) =
  case toUUID (unUserEntityKey uid) of
    Left err -> error $ show err
    Right uuid ->
      User
        (UserId uuid)
        (UserName $ T.unpack $ userEntityName userEntity)
        (UserEmail $ T.unpack $ userEntityEmail userEntity)

-- Convert UserCreate to UserEntity
userCreateToEntity :: UserCreate -> UserEntity
userCreateToEntity (UserCreate (UserName userName) (UserEmail userEmail)) =
  UserEntity
    (T.pack userName)
    (T.pack userEmail)

clapCreateToEntity :: ClapCreate -> ClapEntity
clapCreateToEntity (ClapCreate (UserId uid) cnt) =
  ClapEntity
    cnt
    (UserEntityKey (UUID.toText uid))

clapToEntity :: Clap -> ClapEntity
clapToEntity (Clap (ClapId _) cnt (UserId uid)) =
  ClapEntity
    cnt
    (UserEntityKey (UUID.toText uid))

entityToClap :: Entity ClapEntity -> Clap
entityToClap (Entity uid clapEntity) =
  either (error . show) id $ do
    clapUuid <- toUUID (unClapEntityKey uid)
    userUuid <- toUUID (unUserEntityKey $ clapEntityUserId clapEntity)
    return $
      Clap
        (ClapId clapUuid)
        (clapEntityCount clapEntity)
        (UserId userUuid)
