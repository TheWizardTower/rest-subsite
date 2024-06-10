{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{- |
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
module Lib (
    runServer,
    getHomeR,
    getReadAllTodosR,
    postRestCreateR,
) where

import Control.Monad (join)
import Data.Aeson (FromJSON, ToJSON, Value, toJSON)
import Data.List.NonEmpty (NonEmpty, head, singleton)
import Data.Text (Text, unpack)
import Database.PostgreSQL.Simple (Connection, Only (..), connectPostgreSQL, query, query_)
import Database.PostgreSQL.Simple.FromField
import GHC.Generics
import GHC.Num.Integer (integerFromInt, integerToInt)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..), exitWith)
import Text.Read (readMaybe)
import Yesod
import Yesod.EmbeddedStatic
import Prelude hiding (head)

import Types

mkEmbeddedStatic True "myStatic" [embedDirAt "" "./docs"]

data MyRest = MyRest
    { connPool :: NonEmpty Connection
    -- ^ A pool of connections to the PostgreSQL database.
    , getStatic :: EmbeddedStatic
    -- ^ The static website in the website configuration. This is where
    -- we store the generated haddock documentation.
    }

mkYesod
    "MyRest"
    [parseRoutes|
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

-- | The handler for the base route.
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    toWidget [julius|console.log("Hello, world!");|]
    [whamlet|
          <h1>Hello</h1>
          <p>Check the <a href=@{StaticR index_html}>embedded file</a></p>
          |]

{- | Endpoint to create a TODO. Consumes a JSON body that matches up with the
 CreateTodoRequest type. Returns a CreateTodoResponse with the Todo
 ID, or a FailedToCreateTodo message on failure.
-}
postRestCreateR :: Handler Value
postRestCreateR = do
    myVal :: CreateTodoRequest <- requireCheckJsonBody
    conn <- head <$> getsYesod connPool
    res :: [Only Int] <-
        liftIO $
            query
                conn
                "INSERT INTO todos (title, description, done) VALUES (?, ?, ?) RETURNING id"
                (ctrTitle myVal, ctrDescription myVal, False)

    return $ case res of
        [Only i] -> toJSON $ CreateTodoResponse{ctrTid = i}
        _ -> toJSON $ FailedToCreateTodo{createReq = myVal}

getReadAllTodosR :: Handler Value
getReadAllTodosR = do
    conn <- head <$> getsYesod connPool
    res :: [Only TodosStruct] <-
        liftIO $
            query_ conn "SELECT (title, description, completed, id) FROM todos"
    return $ toJSON $ fmap (toJSON . fromOnly) res

{- | Run the server we've described above. This is called from main.
 The function pulls an environment variable to get the database
 connection string, then builds a "connection pool" (currently, only
 one) for the application to use.
-}
getTodoR :: Int -> Handler Value
getTodoR tid = do
    conn <- getsYesod $ head . connPool
    res :: [Only TodosStruct] <-
        liftIO $
            query
                conn
                "SELECT (title, description, completed, id) FROM todos WHERE id = ?"
                (Only tid)
    return $ case res of
        [] -> toJSON TodoNotFound{tnfTid = tid}
        [tds] -> toJSON $ fromOnly tds
        -- We somehow found multiple matching todos, which should be
        -- impossible in our schema? Abort because reality has
        -- broken and cthulhu is talking to us through our DB.
        _ -> toJSON $ MultipleTodosFound{mtfTid = tid}

putUpdateTodoR :: Int -> Handler Value
putUpdateTodoR tid = do
    utr :: UpdateTodoRequest <- requireCheckJsonBody
    conn <- head <$> getsYesod connPool
    res :: [Only Integer] <-
        liftIO $
            query
                conn
                "UPDATE todos SET title = COALESCE(?, title), description = COALESCE(?, description), done = COALESCE(?, done) where ID = ? RETURNING id"
                (utrTitle utr, utrDescription utr, utrFinished utr, integerFromInt tid)
    return $ case res of
        [] -> toJSON $ TodoNotFound{tnfTid = tid}
        [tid'] -> toJSON $ UpdateTodoResponse{utrTid = integerToInt $ fromOnly tid'}
        -- Again, bail if we updated more than one todo, somehow.
        _ -> toJSON $ MultipleTodosFound{mtfTid = tid}

deleteDelTodoR :: Int -> Handler Value
deleteDelTodoR tid = do
    conn <- head <$> getsYesod connPool
    res :: [Only Int] <-
        liftIO
            $ query
                conn
                "DELETE FROM todos where ID = ? RETURNING id"
            $ Only tid
    return $ case res of
        [] -> toJSON $ TodoNotFound{tnfTid = tid}
        [tid'] -> toJSON $ DeleteTodoResponse{dtrTid = fromOnly tid'}
        -- Again, bail if we updated more than one todo, somehow.
        _ -> toJSON $ MultipleTodosFound{mtfTid = tid}

runServer :: IO ()
runServer = do
    putStrLn "runServer"
    connStrM <- lookupEnv "DATABASE_URL"
    connStr :: String <-
        case connStrM of
            Nothing -> do
                putStrLn "This program requires the DATABASE_URL environment variable to be set with a valid PostgreSQL connection string."
                exitWith $ ExitFailure 127
            Just str -> return str
    putStrLn "Connection string:"
    putStrLn connStr
    conn <- connectPostgreSQL "postgres://postgres:example@localhost:5432/postgres"
    warp 3000 MyRest{connPool = singleton conn, getStatic = myStatic}
