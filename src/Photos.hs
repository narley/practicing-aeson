{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}

module Photos
  ( Photo (..)
  , getPhotos
  , getPhotosById
  , getPhotosByAlbumId
  )
  where

import Relude
import qualified Network.Wreq as NW
import qualified Data.Aeson as AE
import qualified Control.Lens as L
import qualified Data.Text as T
import Config (baseURL)

data Photo = Photo
  { photoAlbumId :: Int
  , photoId :: Int
  , photoTitle :: T.Text
  , photoURL :: T.Text
  , thumbnailURL :: T.Text
  }
  deriving (Show)

instance AE.ToJSON Photo where
  toJSON
    Photo
    { photoAlbumId
    , photoId
    , photoTitle
    , photoURL
    , thumbnailURL
    } = AE.object
    [ "albumId" AE..= photoAlbumId
    , "id" AE..= photoId
    , "title" AE..= photoTitle
    , "url" AE..= photoURL
    , "thumbnailURL" AE..= thumbnailURL
    ]

instance AE.FromJSON Photo where
  parseJSON = AE.withObject "Photo" $ \obj -> do
    photoAlbumId <- obj AE..: "albumId"
    photoId <- obj AE..: "id"
    photoTitle <- obj AE..: "title"
    photoURL <- obj AE..: "url"
    thumbnailURL <- obj AE..: "thumbnailUrl"
    return $ Photo photoAlbumId photoId photoTitle photoURL thumbnailURL

type Photos = [Photo]

getPhotos :: IO Photos
getPhotos = do
  res <- NW.asJSON =<< NW.get (baseURL ++ "/photos")
  return $ res L.^. NW.responseBody

getPhotosById :: Int -> IO Photo
getPhotosById id' = do
  res <- NW.asJSON =<< NW.get (baseURL ++ "/photos/" ++ show id')
  return $ res L.^. NW.responseBody

getPhotosByAlbumId :: Int -> IO Photos
getPhotosByAlbumId albumId' = do
  res <- NW.asJSON =<< NW.get (baseURL ++ "/photos")
  let photos = res L.^. NW.responseBody
  return $ filter (\x -> photoAlbumId x == albumId') photos
