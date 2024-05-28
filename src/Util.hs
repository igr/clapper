module Util
  ( toUUID,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as UUID

-- Convert to UUID from Text
toUUID :: Text -> Either Text UUID
toUUID s = case UUID.fromText s of
  Nothing -> Left . T.pack $ "Invalid UUID: " ++ show s
  Just uuid -> return uuid
