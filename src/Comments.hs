{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}

module Comments
  ( Comment (..)
  , getComments
  , getCommentById
  , getCommentsByPartialEmail
  , getCommentsByPostId
  ) where

import Relude
import qualified Network.Wreq as NW
import qualified Data.Text as T
import qualified Data.Aeson as AE
import qualified Control.Lens as L
import Config (baseURL)



data Comment = Comment
  { commentId :: Int
  , name :: T.Text
  , email :: T.Text
  , body :: T.Text
  }
  deriving (Show)

instance AE.ToJSON Comment where
  toJSON Comment
    { commentId
    , name
    , email
    , body
    } = AE.object
    [ "id" AE..= commentId
    , "name" AE..= name
    , "email" AE..= email
    , "body" AE..= body
    ]

instance AE.FromJSON Comment where
  parseJSON = AE.withObject "Comment" $ \obj -> do
    commentId <- obj AE..: "id"
    name <- obj AE..: "name"
    email <- obj AE..: "email"
    body <- obj AE..: "body"
    return $ Comment commentId name email body

type Comments = [Comment]

getComments :: IO Comments
getComments = do
  resp <- NW.asJSON =<< NW.get (baseURL ++ "/comments")
  return $ resp L.^. NW.responseBody

getCommentById :: Int -> IO Comment
getCommentById id' = do
  resp <- NW.asJSON =<< NW.get (baseURL ++ "/comments/" ++ show id')
  return $ resp L.^. NW.responseBody

getCommentsByPartialEmail :: T.Text -> IO (Comments, Int)
getCommentsByPartialEmail partialEmail = do
  comments <- getComments
  let filteredComments = Relude.filter emailContainsPartial comments
  return (filteredComments, Relude.length filteredComments)
  where
    lowerEmail c = T.toLower (email c)
    emailContainsPartial c = T.toLower partialEmail `T.isInfixOf` lowerEmail c

getCommentsByPostId :: Int -> IO Comments
getCommentsByPostId id' = do
  res <- NW.asJSON =<< NW.get (baseURL ++ "/comments?postId=" ++ show id')
  return $ res L.^. NW.responseBody
