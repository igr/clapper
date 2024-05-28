{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Logger (runStderrLoggingT)
import Data.String.Conversions (cs)
import Database.Persist.Sql
  ( ConnectionPool,
    runSqlPool,
  )
import Database.Persist.Sqlite
  ( createSqlitePool,
    runMigration,
  )
import Db.Entities (migrateAll)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import Web.Api

main :: IO ()
main = run 8080 =<< makeApp "clapper.db"

makeApp :: FilePath -> IO Application
makeApp sqliteFile = do
  -- initialize database connection
  pool <- runStderrLoggingT $ createSqlitePool (cs sqliteFile) 5
  runSqlPool (runMigration migrateAll) pool
  return $ simpleCors $ app pool

-- 'serve' comes from Servant and hands a WAI Application:
-- an "abstract" web application, not yet a webserver.
app :: ConnectionPool -> Application
app pool = serve api (server pool)

api :: Proxy API
api = Proxy
