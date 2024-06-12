{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Types and derivations for the example REST endpoint
Copyright: (c) 2024 Adam McCullough
License: GPL-3
Maintainer: merlinfmct87@gmail.com
Stability: experimental
Portability: POSIX

The types that are used in Lib are split out here, to make both files
a bit less noisy and easier to work with.
-}
module Types (
    CreateTodoRequest (..),
    CreateTodoResponse (..),
    FailedToCreateTodo (..),
    TodosStruct (..),
    TodoNotFound (..),
    MultipleTodosFound (..),
    UpdateTodoRequest (..),
    UpdateTodoResponse (..),
    DeleteTodoResponse (..),
) where

import Data.Aeson
import Data.Text
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import GHC.Generics

{- | This is the internal representation of the components of the todo
 create request.
-}
data CreateTodoRequest = CreateTodoRequest
    { ctrTitle :: Text
    -- ^ Title of the todo. Quick, snappy description of the task.
    , ctrDescription :: Text
    -- ^ More in-depth description of the work to be done.
    }
    deriving (Eq, Ord, Show, Generic)

instance FromJSON CreateTodoRequest where
    parseJSON (Object v) = CreateTodoRequest <$> v .: "title" <*> v .: "description"

instance ToJSON CreateTodoRequest where
    toJSON (CreateTodoRequest ctrTitle ctrDescription) = object ["title" .= ctrTitle, "description" .= ctrDescription]

{- | Internal representation of the response of the create todo
 request. If successful, this will return the todo ID for the user to
 refer to when they want to refer to or update the todo later.
-}
data CreateTodoResponse = CreateTodoResponse
    { ctrTid :: Int
    -- ^ ID of the todo that was created.
    }
    deriving (Eq, Ord, Show)

instance FromJSON CreateTodoResponse where
    parseJSON (Object v) = CreateTodoResponse <$> v .: "tid"

instance ToJSON CreateTodoResponse where
    toJSON (CreateTodoResponse ctrTid) = object ["tid" .= ctrTid]

{- | If the create request fails, return the request that failed. Don't
 expose the internal error, as that's a security vulnerability. But, we
 should add some facility to communicate broadly what went wrong,
 especially if the failure was due to invalid inputs.
-}
data FailedToCreateTodo = FailedToCreateTodo
    { createReq :: CreateTodoRequest
    -- ^ The originating create request type.
    }
    deriving (Eq, Ord, Show)

instance FromJSON FailedToCreateTodo where
    parseJSON (Object v) = FailedToCreateTodo <$> v .: "createReq"

instance ToJSON FailedToCreateTodo where
    toJSON (FailedToCreateTodo createReq) = object ["createReq" .= createReq]

{- | This struct is used to represent the result of a read request. As
such, it has the title, description, and completed fields you'd
expect, as well as the todo ID.
-}
data TodosStruct = TodosStruct
    { todoTitle :: Text
    -- ^ Title of the todo. Quick, snappy description of the task.
    , todoDescription :: Text
    -- ^ More in-depth description of the work to be done.
    , todoDone :: Bool
    -- ^ Has the task been completed? True for yes.
    , todoId :: Int
    -- ^ What ID did the database assign to this task?
    }
    deriving (Eq, Ord, Show)

instance FromJSON TodosStruct where
    parseJSON (Object v) = TodosStruct <$> v .: "title" <*> v .: "description" <*> v .: "done" <*> v .: "tid"

instance ToJSON TodosStruct where
    toJSON (TodosStruct tTitle tDescription tDone tId) = object ["title" .= tTitle, "description" .= tDescription, "done" .= tDone, "tid" .= tId]

{- | This struct is returned as a JSON object if we try to read,
update, or delete a todo ID that cannot be found.
-}
data TodoNotFound = TodoNotFound
    { tnfTid :: Int
    -- ^ The TID that we could not find in our data store.
    }
    deriving (Eq, Ord, Show)

instance FromJSON TodoNotFound where
    parseJSON (Object v) = TodoNotFound <$> v .: "tid"

instance ToJSON TodoNotFound where
    toJSON (TodoNotFound tid) = object ["tid" .= tid]

{- | We return this type if we run afoul of the invariant that each
todo has a unique ID. This really only exists to handle a circumstance
that the types allow, but @should@ be impossible.
-}
data MultipleTodosFound = MultipleTodosFound
    { mtfTid :: Int
    -- ^ The TID that somehow was found more than once.
    }
    deriving (Eq, Ord, Show)

instance FromJSON MultipleTodosFound where
    parseJSON (Object v) = MultipleTodosFound <$> v .: "tid"

instance ToJSON MultipleTodosFound where
    toJSON (MultipleTodosFound mTid) = object ["tid" .= mTid]

{- | An update request. If a field is Nothing, then it will be
unchanged in the request. Note that this allows for a "no-op" update,
but having the types constrain that "at least one field shall be
populated" would be ... tricky.
-}
data UpdateTodoRequest = UpdateTodoRequest
    { utrTitle :: Maybe Text
    -- ^ A possible change to a title.
    , utrDescription :: Maybe Text
    -- ^ A possible change to the description.
    , utrFinished :: Maybe Bool
    -- ^ A possible change to the finished state.
    }
    deriving (Eq, Ord, Show)

instance FromJSON UpdateTodoRequest where
    parseJSON (Object v) = UpdateTodoRequest <$> v .:? "title" <*> v .:? "description" <*> v .:? "done"

instance ToJSON UpdateTodoRequest where
    toJSON (UpdateTodoRequest uTitle uDescription uFinished) = object ["title" .= uTitle, "description" .= uDescription, "done" .= uFinished]

{- | The response returned -- as a JSON object -- on successfully
updating a todo entry.
-}
data UpdateTodoResponse = UpdateTodoResponse
    { utrTid :: Int
    -- ^ Update was successful, return TID update was performed on
    }
    deriving (Eq, Ord, Show)

instance FromJSON UpdateTodoResponse where
    parseJSON (Object v) = UpdateTodoResponse <$> v .: "tid"

instance ToJSON UpdateTodoResponse where
    toJSON (UpdateTodoResponse uTid) = object ["tid" .= uTid]

{- | The response returned -- as a JSON object -- on successfully
deleting a todo entry. Hopefully one we've finished, rather than
abandoned.
-}
data DeleteTodoResponse = DeleteTodoResponse
    { dtrTid :: Int
    -- ^ Update was successful, return TID update was performed on
    }
    deriving (Eq, Ord, Show)

instance FromJSON DeleteTodoResponse where
    parseJSON (Object v) = DeleteTodoResponse <$> v .: "tid"

instance ToJSON DeleteTodoResponse where
    toJSON (DeleteTodoResponse dTid) = object ["tid" .= dTid]
