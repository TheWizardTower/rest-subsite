{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types (InvalidRequest(..), CreateTodoRequest(..), CreateTodoResponse(..), FailedToCreateTodo(..), TodosStruct(..), TodoNotFound(..), MultipleTodosFound(..), UpdateTodoRequest(..), UpdateTodoResponse(..), DeleteTodoResponse(..)) where

import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Data.Text
import GHC.Generics

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

data TodoNotFound = TodoNotFound
  { -- | The TID that we could not find in our data store.
    tnfTid :: Int
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data MultipleTodosFound = MultipleTodosFound
  { -- | The TID that somehow was found more than once.
    mtfTid :: Int
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)


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

data DeleteTodoResponse = DeleteTodoResponse
  { -- | Update was successful, return TID update was performed on
    dtrTid :: Int
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)
