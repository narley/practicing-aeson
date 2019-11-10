{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Albums
  ( Album (..)
  , getAlbums
  , getAlbumById
  )
  where

import Relude hiding (get)
import Network.Wreq as WR
import qualified Data.Aeson as AE
import qualified Data.Text as T
import qualified Control.Lens as L hiding ((.=))
import GHC.Generics
import Config (baseURL)


data Album = Album
  { userId :: Int
  , albumId :: Int
  , albumTitle :: T.Text
  }
  deriving (Show)

instance AE.ToJSON Album where
  toJSON Album
    { userId = userId'
    , albumId = id'
    , albumTitle = albumTitle'
    } = AE.object
    [ "userId" AE..= userId'
    , "id" AE..= id'
    , "title" AE..= albumTitle'
    ]

instance AE.FromJSON Album where
  parseJSON = AE.withObject "Album" $ \obj -> do
    userId' <- obj AE..: "userId"
    albumId' <- obj AE..: "id"
    albumTitle' <- obj AE..: "title"
    return $ Album userId' albumId' albumTitle'

type Albums = [Album]


getAlbums :: IO Albums
getAlbums = do
  resp <- asJSON =<< get (baseURL ++ "/albums")
  return $ resp L.^. responseBody

getAlbumById :: Int -> IO Album
getAlbumById id' = do
  res <- asJSON =<< get (baseURL ++ "/albums/" ++ show id')
  return $ res L.^. responseBody
