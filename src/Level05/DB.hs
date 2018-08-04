{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Level05.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Bifunctor                     (first)
import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection,
                                                     Query (fromQuery))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level05.Types                      (Comment, CommentText,
                                                     Error (DBError), Topic,
                                                     fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           Level05.AppM                       (AppM, liftEither)

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB ::
  FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB ::
  FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDB ::
  (a -> Either Error b)
  -> IO a
  -> AppM b
runDB f q = do
  safeResult <- liftIO (first DBError <$> Sql.runDBAction q)
  liftEither (f =<< safeResult)

getComments ::
  FirstAppDB
  -> Topic
  -> AppM [Comment]
getComments (FirstAppDB conn) topic =
  runDB (traverse fromDBComment) query
  where
    selectCommentsQ = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
    topicParam = Sql.Only (getTopic topic)
    query = Sql.query conn selectCommentsQ topicParam

addCommentToTopic ::
  FirstAppDB
  -> Topic
  -> CommentText
  -> AppM ()
addCommentToTopic (FirstAppDB conn) topic comment =
  runDB pure query
  where
    addCommentQ = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
    params = (getTopic topic, getCommentText comment,) <$> getCurrentTime
    query = Sql.execute conn addCommentQ =<< params

getTopics ::
  FirstAppDB
  -> AppM [Topic]
getTopics (FirstAppDB conn) =
  runDB (traverse (mkTopic . Sql.fromOnly)) query
  where
    selectUniqTopicsQ = "SELECT DISTINCT topic FROM comments"
    query = Sql.query_ conn selectUniqTopicsQ

deleteTopic ::
  FirstAppDB
  -> Topic
  -> AppM ()
deleteTopic (FirstAppDB conn) topic =
  runDB pure query
  where
    deleteTopicCommentsQ = "DELETE FROM comments WHERE topic = ?"
    params = Sql.Only (getTopic topic)
    query = Sql.execute conn deleteTopicCommentsQ params
