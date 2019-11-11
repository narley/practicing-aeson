{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}

module Albums
  ( Album (..)
  , getAlbums
  , getAlbumById
  )
  where

import Relude
import qualified Network.Wreq as NW
import qualified Data.Aeson as AE
import qualified Data.Text as T
import qualified Control.Lens as L
import Config (baseURL)


data Album = Album
  { userId :: Int
  , albumId :: Int
  , albumTitle :: T.Text
  }
  deriving (Show)

instance AE.ToJSON Album where
  toJSON Album
    { userId
    , albumId
    , albumTitle
    } = AE.object
    [ "userId" AE..= userId
    , "id" AE..= albumId
    , "title" AE..= albumTitle
    ]

instance AE.FromJSON Album where
  parseJSON = AE.withObject "Album" $ \obj -> do
    userId <- obj AE..: "userId"
    albumId <- obj AE..: "id"
    albumTitle <- obj AE..: "title"
    return $ Album userId albumId albumTitle

type Albums = [Album]


getAlbums :: IO Albums
getAlbums = do
  resp <- NW.asJSON =<< NW.get (baseURL ++ "/albums")
  return $ resp L.^. NW.responseBody

getAlbumById :: Int -> IO Album
getAlbumById id' = do
  res <- NW.asJSON =<< NW.get (baseURL ++ "/albums/" ++ show id')
  return $ res L.^. NW.responseBody
