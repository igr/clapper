{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Web.Api
  ( API,
    server,
  )
where

import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sql (ConnectionPool, SqlPersistM)
import Db.Repository
import Db.RunSql (runSql)
import Domain.Clap
import Domain.User
import Servant
import Web.ApiData

type API =
  -- /api/users
  "api" :> "users" :> Get '[JSON] [User]
    :<|> "api" :> "users" :> ReqBody '[JSON] UserCreate :> Post '[JSON] User
    :<|> "api" :> "users" :> Capture "userId" PathUserId :> Get '[JSON] (Maybe User)
    -- /api/claps
    :<|> "api" :> "claps" :> Get '[JSON] [Clap]
    :<|> "api" :> "claps" :> ReqBody '[JSON] ClapCreate :> Post '[JSON] Clap
    :<|> "api" :> "claps" :> Capture "clapId" PathClapId :> Get '[JSON] (Maybe Clap)
    :<|> "api" :> "claps" :> Capture "clapId" PathClapId :> "increment" :> Post '[JSON] (Maybe Clap)

server :: ConnectionPool -> Server API
server pool =
  getUsers
    :<|> postUser
    :<|> getUser
    :<|> getClaps
    :<|> postClap
    :<|> getClap
    :<|> incrementClap
  where
    runDbAction :: SqlPersistM a -> Handler a
    runDbAction action = liftIO $ runSql action pool

    getUsers :: Handler [User]
    getUsers = runDbAction $ do
      fetchAllUsers

    postUser :: UserCreate -> Handler User
    postUser user = runDbAction $ do
      storeUser user

    getUser :: PathUserId -> Handler (Maybe User)
    getUser uid = runDbAction $ do
      fetchUser $ extractUserId uid

    getClaps :: Handler [Clap]
    getClaps = runDbAction $ do
      fetchAllClaps

    postClap :: ClapCreate -> Handler Clap
    postClap clap = runDbAction $ do
      storeClap clap

    getClap :: PathClapId -> Handler (Maybe Clap)
    getClap cid = runDbAction $ do
      fetchClap $ extractClapId cid

    incrementClap :: PathClapId -> Handler (Maybe Clap)
    incrementClap cid = runDbAction $ do
      -- >>= is used to chain the operations, handling Nothing automatically.
      fetchClap (extractClapId cid) >>= traverse updateAndReturn
      where
        updateAndReturn clap = do
          let clap' = clap {count = count clap + 1}
          key <- updateClap (extractClapId cid) clap'
          fetchExistingClap key
