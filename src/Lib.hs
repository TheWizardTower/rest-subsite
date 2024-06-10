{-|
Module: Example REST endpoint
Copyright: (c) 2024 Adam McCullough
License: GPL-3
Maintainer: merlinfmct87@gmail.com
Stability: experimental
Portability: POSIX

This is an experimental playground for how to maintain documentation
for a REST endpoint and associated datatypes, and hosting the
associated documentation for both.
-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Lib
    ( runServer
    , CreateTodoRequest(..)
    , CreateTodoResponse(..)
    , FailedToCreateTodo(..)
    , TodosStruct(..) 
    , getHomeR
    , getReadAllTodosR
    , postRestCreateR
    ) where

import Control.Monad (join)
import Data.Aeson
import Data.Text
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import GHC.Num.Integer (integerFromInt, integerToInt)
import GHC.Generics
import System.Environment
import Text.Read (readMaybe)
import Yesod
import Yesod.EmbeddedStatic
 
mkEmbeddedStatic False "myStatic" [embedDirAt "" "./docs" ]
  
data MyRest = MyRest
  { -- | A pool of connections to the PostgreSQL database.
    connPool :: [Connection] 
  -- | The static website in the website configuration. This is where
  -- we store the generated haddock documentation.
  , getStatic :: EmbeddedStatic
  }

mkYesod "MyRest" [parseRoutes|
/                 HomeR                  GET
/create           RestCreateR            POST
/getAll           ReadAllTodosR          GET
/getTodo/#Int     TodoR                  GET
/updateTodo/#Int  UpdateTodoR            PUT
/deleteTodo/#Int  DelTodoR               DELETE
/static           StaticR EmbeddedStatic getStatic
|]

instance Yesod MyRest where
  addStaticContent = embedStaticContent getStatic StaticR Right

-- |The handler for the base route.
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  toWidget [julius|console.log("Hello, world!");|]
  [whamlet|
          <h1>Hello</h1>
          <p>Check the <a href=@{StaticR index_html}>embedded file</a></p>
          |]

data InvalidRequest = InvalidRequest
  { -- | What you did wrong, in a string.
    errMsg :: Text
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- |This is the internal representation of the components of the todo
-- create request.
data CreateTodoRequest = CreateTodoRequest
  { -- | Title of the todo. Quick, snappy description of the task. 
    ctrTitle :: Text
    -- | More in-depth description of the work to be done.
  , ctrDescription :: Text
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- |Internal representation of the response of the create todo
-- request. If successful, this will return the todo ID for the user to
-- refer to when they want to refer to or update the todo later.
data CreateTodoResponse = CreateTodoResponse
  { -- | ID of the todo that was created.
    ctrTid :: Int
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- |If the create request fails, return the request that failed. Don't
-- expose the internal error, as that's a security vulnerability. But, we
-- should add some facility to communicate broadly what went wrong,
-- especially if the failure was due to invalid inputs.
data FailedToCreateTodo = FailedToCreateTodo
  { -- | The originating create request type.
    createReq :: CreateTodoRequest
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- |Endpoint to create a TODO. Consumes a JSON body that matches up with the 
-- CreateTodoRequest type. Returns a CreateTodoResponse with the Todo
-- ID, or a FailedToCreateTodo message on failure.
postRestCreateR :: Handler Value
postRestCreateR = do
  myVal :: CreateTodoRequest <- requireCheckJsonBody
  conn <- Prelude.head <$> getsYesod connPool
  res :: [Only Int] <-
    liftIO $
      query conn
        "INSERT INTO todos (title, description, done) VALUES (?, ?, ?) RETURNING id"
        (ctrTitle myVal, ctrDescription myVal, False)

  return $ case res of
              [Only i] -> toJSON $ CreateTodoResponse { ctrTid = i }
              _        -> toJSON $ FailedToCreateTodo { createReq = myVal }

-- | This struct is used to represent the result of a read request. As
-- such, it has the title, description, and completed fields you'd
-- expect, as well as the todo ID.
data TodosStruct = TodosStruct
  { -- | Title of the todo. Quick, snappy description of the task. 
    todoTitle :: Text
    -- | More in-depth description of the work to be done.
  ,  todoDescription :: Text
    -- | Has the task been completed? True for yes.
  , todoCompleted :: Bool
    -- | What ID did the database assign to this task?
  , tsTid :: Int
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, FromRow, ToRow, FromField)

getReadAllTodosR :: Handler Value 
getReadAllTodosR = do
  conn <- Prelude.head <$> getsYesod connPool
  res :: [Only TodosStruct] <- liftIO $
    query_ conn "SELECT (title, description, completed, id) FROM todos"
  return $ toJSON $ fmap (toJSON . fromOnly) res 
  
-- |Run the server we've described above. This is called from main.
-- The function pulls an environment variable to get the database
-- connection string, then builds a "connection pool" (currently, only
-- one) for the application to use.

data TodoNotFound = TodoNotFound
  { -- | The TID that we could not find in our data store.
    tnfTid :: Int
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data MultipleTodosFound = MultipleTodosFound
  { -- | The TID that somehow was found more than once.
    mtfTid :: Int
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

getTodoR :: Int ->  Handler Value
getTodoR tid = do
  conn <- getsYesod $ Prelude.head . connPool
  res :: [Only TodosStruct] <- liftIO $
    query conn
      "SELECT (title, description, completed, id) FROM todos WHERE id = ?"
      (Only tid)
  return $ case res of
          [] -> toJSON TodoNotFound { tnfTid = tid }
          [tds] -> toJSON $ fromOnly tds
          -- We somehow found multiple matching todos, which should be
          -- impossible in our schema? Abort because reality has
          -- broken and cthulhu is talking to us through our DB.
          _ -> toJSON $ MultipleTodosFound { mtfTid = tid }

data UpdateTodoRequest = UpdateTodoRequest
  { -- | A possible change to a title.
    utrTitle :: Maybe Text
    -- | A possible change to the description.
  , utrDescription :: Maybe Text
    -- | A possible change to the finished state.
  , utrFinished :: Maybe Bool
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data UpdateTodoResponse = UpdateTodoResponse
  { -- | Update was successful, return TID update was performed on
    utrTid :: Int
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

putUpdateTodoR :: Int -> Handler Value
putUpdateTodoR tid = do
  utr :: UpdateTodoRequest <- requireCheckJsonBody
  conn <- Prelude.head <$> getsYesod connPool
  res :: [Only Integer] <- liftIO $ query conn
    "UPDATE todos SET title = COALESCE(?, title), description = COALESCE(?, description), done = COALESCE(?, done) where ID = ? RETURNING id"
    (utrTitle utr, utrDescription utr, utrFinished utr, integerFromInt tid)
  return $ case res of
    [] -> toJSON $ TodoNotFound { tnfTid = tid }
    [tid'] -> toJSON $ UpdateTodoResponse { utrTid = integerToInt $ fromOnly tid' }
    -- Again, bail if we updated more than one todo, somehow.
    _ -> toJSON $ MultipleTodosFound { mtfTid = tid }

data DeleteTodoResponse = DeleteTodoResponse
  { -- | Update was successful, return TID update was performed on
    dtrTid :: Int
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

deleteDelTodoR :: Int -> Handler Value
deleteDelTodoR tid = do
  conn <- Prelude.head <$> getsYesod connPool
  res :: [Only Int] <- liftIO $  query conn
    "DELETE FROM todos where ID = ? RETURNING id"
    $ Only tid
  return $ case res of
    [] -> toJSON $ TodoNotFound { tnfTid = tid }
    [tid'] -> toJSON $ DeleteTodoResponse { dtrTid = fromOnly tid' }
    -- Again, bail if we updated more than one todo, somehow.
    _ -> toJSON $ MultipleTodosFound { mtfTid = tid }


runServer :: IO ()
runServer = do
  putStrLn "runServer"
  connStrM <- lookupEnv "DATABASE_URL"
  let
    connStr :: String
    connStr =
        case connStrM of
            Nothing -> "postgres://postgres:example@localhost:5432/postgres"
            Just str -> str
  putStrLn "Connection string:"
  putStrLn connStr
  conn <- connectPostgreSQL "postgres://postgres:example@localhost:5432/postgres"
  warp 3000 MyRest { connPool = [conn], getStatic = myStatic }

