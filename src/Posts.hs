{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}

module Posts
  ( Post (..)
  , getPosts
  , getPostById
  , getPostsByUserId
  ) where

import Relude
import qualified Network.Wreq as NW
import qualified Data.Text as T
import qualified Data.Aeson as AE
import qualified Control.Lens as L
import Config (baseURL)



data Post = Post
  { userId :: Int
  , postId :: Int
  , title :: T.Text
  , body :: T.Text
  }
  deriving (Show, Generic)

type Posts = [Post]

instance AE.FromJSON Post where
  parseJSON = AE.withObject "Post" $ \obj -> do
    userId <- obj AE..: "userId"
    postId <- obj AE..: "id"
    title <- obj AE..: "title"
    body <- obj AE..: "body"
    return (Post userId postId title body)
-- Or using <$> and <*> when all the data fields match the JSON key fields
-- <$> :: Function f => (a -> b) -> fa -> fb
-- <*> :: Applicative f => f (a -> b) -> fa -> fb
-- instance FromJSON Post where
-- parseJSON = withObject "Post" $ \obj -> -- don't need "do" notation anymore
--   -- this will return "Post userId"
--   Post <$> obj .: "userId"
--         -- this will return "Post userId id"
--        <*> obj .: "id"
--         -- this will return "Post userId id title"
--        <*> obj .: "title"
--         -- and finally return the entire construction "Post userId id title body"
--        <*> obj .: "body"

instance AE.ToJSON Post where
  toJSON Post
    { userId
    , postId
    , title
    , body
    } =
    AE.object [ "userId" AE..= userId
           , "id" AE..= postId
           , "title" AE..= title
           , "body" AE..= body
           ]

getPosts :: IO Posts
getPosts = do
  res <- NW.asJSON =<< NW.get (baseURL ++ "/posts")
  return $ res L.^. NW.responseBody

getPostById :: Int -> IO Post
getPostById postId' = do
  res <- NW.asJSON =<< NW.get (baseURL ++ "/posts/" ++ show postId')
  return $ res L.^. NW.responseBody

getPostsByUserId :: Int -> IO Posts
getPostsByUserId id' = do
  -- Using provided API ...
  -- res <- NW.asJSON =<< NW.get (baseURL ++ "/posts?userId=" ++ show userId)
  -- Or using filter ...
  res <- NW.asJSON =<< NW.get (baseURL ++ "/posts")
  resBody <- pure $ res L.^. NW.responseBody
  return $ Relude.filter (\p -> id' == userId p) resBody
