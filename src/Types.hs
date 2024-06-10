{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

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
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

{- | Internal representation of the response of the create todo
 request. If successful, this will return the todo ID for the user to
 refer to when they want to refer to or update the todo later.
-}
data CreateTodoResponse = CreateTodoResponse
    { ctrTid :: Int
    -- ^ ID of the todo that was created.
    }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

{- | If the create request fails, return the request that failed. Don't
 expose the internal error, as that's a security vulnerability. But, we
 should add some facility to communicate broadly what went wrong,
 especially if the failure was due to invalid inputs.
-}
data FailedToCreateTodo = FailedToCreateTodo
    { createReq :: CreateTodoRequest
    -- ^ The originating create request type.
    }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

{- | This struct is used to represent the result of a read request. As
such, it has the title, description, and completed fields you'd
expect, as well as the todo ID.
-}
data TodosStruct = TodosStruct
    { todoTitle :: Text
    -- ^ Title of the todo. Quick, snappy description of the task.
    , todoDescription :: Text
    -- ^ More in-depth description of the work to be done.
    , todoCompleted :: Bool
    -- ^ Has the task been completed? True for yes.
    , tsTid :: Int
    -- ^ What ID did the database assign to this task?
    }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, FromRow, ToRow, FromField)

{- | This struct is returned as a JSON object if we try to read,
update, or delete a todo ID that cannot be found.
-}
data TodoNotFound = TodoNotFound
    { tnfTid :: Int
    -- ^ The missing Todo ID
    }
    -- \^ The TID that we could not find in our data store.

    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

{- | We return this type if we run afoul of the invariant that each
todo has a unique ID. This really only exists to handle a circumstance
that the types allow, but @should@ be impossible.
-}
data MultipleTodosFound = MultipleTodosFound
    { mtfTid :: Int
    -- ^ The TID that somehow was found more than once.
    }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

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
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

{- | The response returned -- as a JSON object -- on successfully
updating a todo entry.
-}
data UpdateTodoResponse = UpdateTodoResponse
    { utrTid :: Int
    -- ^ Update was successful, return TID update was performed on
    }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

{- | The response returned -- as a JSON object -- on successfully
deleting a todo entry. Hopefully one we've finished, rather than
abandoned.
-}
data DeleteTodoResponse = DeleteTodoResponse
    { dtrTid :: Int
    -- ^ Update was successful, return TID update was performed on
    }
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)
