{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}


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
  { userId :: Int
  , title :: T.Text
  , completed :: Bool
  }
  deriving (Show, Generic, AE.ToJSON, AE.FromJSON)

type Todos = [Todo]

getTodos :: IO Todos
getTodos = do
  resp <- NW.asJSON =<< NW.get (baseURL ++ "/todos")
  return $ resp L.^. NW.responseBody

getTodoById :: Int -> IO Todo
getTodoById id' = do
  resp <- NW.asJSON =<< NW.get (baseURL ++ "/todos/" ++ show id')
  return $ resp L.^. NW.responseBody
