{-# LANGUAGE DataKinds #-}

module Db.Repository
  ( fetchAllUsers,
    fetchUser,
    fetchClap,
    fetchExistingClap,
    fetchAllClaps,
    storeClap,
    storeUser,
    updateClap,
    unClapEntityKey,
  )
where

import Control.Monad.IO.Class (MonadIO)
import Database.Persist
import Db.Entities
import Db.RunSql (DbAction)
import Domain.Clap
import Domain.User

-- Fetch all users from the database
fetchAllUsers :: (MonadIO m) => DbAction m [User]
fetchAllUsers = do
  userEntities <- selectList [] []
  return $ map entityToUser userEntities

-- Fetch single user
fetchUser :: (MonadIO m) => UserId -> DbAction m (Maybe User)
fetchUser uid = do
  let key = userIdToKey uid
  userEntity <- get key
  return $ fmap (entityToUser . Entity key) userEntity

-- Insert a user into the database
storeUser :: (MonadIO m) => UserCreate -> DbAction m User
storeUser user = do
  let userEntity = userCreateToEntity user
  key <- generateUserEntityKey
  insertKey key userEntity
  maybeUser <- get key
  case maybeUser of
    Nothing -> error "User not found"
    Just userEntity' -> return $ (entityToUser . Entity key) userEntity'

-- Insert a new clap record into the database
storeClap :: (MonadIO m) => ClapCreate -> DbAction m Clap
storeClap clap = do
  let clapEntity = clapCreateToEntity clap
  key <- generateClapEntityKey
  insertKey key clapEntity
  maybeClap <- get key
  case maybeClap of
    Nothing -> error "Clap not found"
    Just clapEntity' -> return $ (entityToClap . Entity key) clapEntity'

updateClap :: (MonadIO m) => ClapId -> Clap -> DbAction m ClapId
updateClap cid newClap = do
  let key = clapIdToKey cid
  let clap = clapToEntity newClap
  replace key clap
  return cid

-- Fetch all users from the database
fetchAllClaps :: (MonadIO m) => DbAction m [Clap]
fetchAllClaps = do
  clapEntities <- selectList [] []
  return $ map entityToClap clapEntities

-- Fetch single clap
fetchClap :: (MonadIO m) => ClapId -> DbAction m (Maybe Clap)
fetchClap cid = do
  let key = clapIdToKey cid
  clapEntity <- get key
  return $ fmap (entityToClap . Entity key) clapEntity

-- Fetch single clap
fetchExistingClap :: (MonadIO m) => ClapId -> DbAction m Clap
fetchExistingClap cid = do
  maybeClap <- fetchClap cid
  case maybeClap of
    Nothing -> error "Clap not found"
    Just clap -> return clap
