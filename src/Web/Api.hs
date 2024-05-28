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
import Database.Persist.Sql (ConnectionPool)
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
    getUsers :: Handler [User]
    getUsers = liftIO $ flip runSql pool $ do
      fetchAllUsers

    postUser :: UserCreate -> Handler User
    postUser user = liftIO $ flip runSql pool $ do
      storeUser user

    getUser :: PathUserId -> Handler (Maybe User)
    getUser uid = liftIO $ flip runSql pool $ do
      fetchUser $ extractUserId uid

    getClaps :: Handler [Clap]
    getClaps = liftIO $ flip runSql pool $ do
      fetchAllClaps

    postClap :: ClapCreate -> Handler Clap
    postClap clap = liftIO $ flip runSql pool $ do
      storeClap clap

    getClap :: PathClapId -> Handler (Maybe Clap)
    getClap cid = liftIO $ flip runSql pool $ do
      fetchClap $ extractClapId cid

    incrementClap :: PathClapId -> Handler (Maybe Clap)
    incrementClap cid = liftIO $ flip runSql pool $ do
      -- >>= is used to chain the operations, handling Nothing automatically.
      fetchClap (extractClapId cid) >>= traverse updateAndReturn
      where
        updateAndReturn clap = do
          let clap' = clap {count = count clap + 1}
          key <- updateClap (extractClapId cid) clap'
          fetchExistingClap key
