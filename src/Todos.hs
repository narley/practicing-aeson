{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}

module Todos
  ( getTodos
  , getTodoById
  , Todo (..)
  ) where

import Relude
import qualified Network.Wreq as NW
import qualified Data.Text as T
import qualified Data.Aeson as AE
import qualified Control.Lens as L
import Config (baseURL)


data Todo = Todo
  { todoUserId :: Int
  , todoTitle :: T.Text
  , todoCompleted :: Bool
  }
  deriving (Show)

instance AE.FromJSON Todo where
  parseJSON = AE.withObject "Todo" $ \obj -> do
    todoUserId <- obj AE..: "userId"
    todoTitle <- obj AE..: "title"
    todoCompleted <- obj AE..: "completed"
    return $ Todo todoUserId todoTitle todoCompleted

instance AE.ToJSON Todo where
  toJSON Todo
    { todoUserId
    , todoTitle
    , todoCompleted
    } = AE.object
        [ "userId" AE..= todoUserId
        , "title" AE..= todoTitle
        , "completed" AE..= todoCompleted
        ]

type Todos = [Todo]

getTodos :: IO Todos
getTodos = do
  resp <- NW.asJSON =<< NW.get (baseURL ++ "/todos")
  return $ resp L.^. NW.responseBody

getTodoById :: Int -> IO Todo
getTodoById id' = do
  resp <- NW.asJSON =<< NW.get (baseURL ++ "/todos/" ++ show id')
  return $ resp L.^. NW.responseBody
