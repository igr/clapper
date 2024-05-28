module Db.RunSql
  ( runSql,
    DbAction,
  )
where

import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Database.Persist.Sql (SqlBackend, runSqlPersistMPool)
import Database.Persist.Sqlite (ConnectionPool)

-- Type alias for convenience
type DbAction m a = ReaderT SqlBackend m a

-- Run database queries with a connection pool
runSql :: DbAction (NoLoggingT (ResourceT IO)) a -> ConnectionPool -> IO a
runSql = runSqlPersistMPool
