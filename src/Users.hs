{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}

module Users
  ( User (..)
  , Address (..)
  , Geo (..)
  , getUsers
  , getUserById
  , filterUsersByCity
  )
  where

import Relude
import qualified Network.Wreq as NW
import qualified Data.Text as T
import qualified Data.Aeson as AE
import qualified Control.Lens as L
import Config (baseURL)


data User = User
  { userId :: Int
  , userName :: T.Text
  , userEmail :: T.Text
  , address :: Address
  , userPhone :: T.Text
  , userWebsite :: T.Text
  , userCompany :: Company
  }
  deriving (Show)

type Users = [User]

instance AE.ToJSON User where
  toJSON User
    { userId
    , userName
    , userEmail
    , address
    , userPhone
    , userWebsite
    , userCompany
    }
    = AE.object
        [ "id" AE..= userId
        , "name" AE..= userName
        , "email" AE..= userEmail
        , "address" AE..= address
        , "phone" AE..= userPhone
        , "website" AE..= userWebsite
        , "company" AE..= userCompany
        ]

instance AE.FromJSON User where
  parseJSON = AE.withObject "User" $ \obj -> do
    userId <- obj AE..: "id"
    userName <- obj AE..: "username"
    userEmail <- obj AE..: "email"
    address <- obj AE..: "address"
    userPhone <- obj AE..: "phone"
    userWebsite <- obj AE..: "website"
    userCompany <- obj AE..: "company"
    return $ User userId userName userEmail address userPhone userWebsite userCompany

data Address = Address
  { street :: T.Text
  , suite :: T.Text
  , city :: T.Text
  , postcode :: T.Text
  , geo :: Geo
  }
  deriving (Show)

instance AE.ToJSON Address where
  toJSON Address
    { street
    , suite
    , city
    , postcode
    , geo
    } = AE.object
        [ "street" AE..= street
        , "suite" AE..= suite
        , "city" AE..= city
        , "zipcode" AE..= postcode
        , "geo" AE..= geo
        ]

instance AE.FromJSON Address where
  parseJSON = AE.withObject "Address" $ \obj -> do
    street <- obj AE..: "street"
    suite <- obj AE..: "suite"
    city <- obj AE..: "city"
    postcode <- obj AE..: "zipcode"
    geo <- obj AE..: "geo"
    return $ Address street suite city postcode geo

data Geo = Geo
  { lat :: T.Text
  , lng :: T.Text
  }
  deriving (Show)

instance AE.ToJSON Geo where
  toJSON Geo
    { lat
    , lng
    } = AE.object
        [ "lat" AE..= lat
        , "lng" AE..= lng
        ]

instance AE.FromJSON Geo where
  parseJSON = AE.withObject "Geo" $ \obj -> do
    lat <- obj AE..: "lat"
    lng <- obj AE..: "lng"
    return $ Geo lat lng

data Company = Company
  { companyName :: T.Text
  , catchPhrase :: T.Text
  , bs :: T.Text
  }
  deriving (Show)

instance AE.ToJSON Company where
  toJSON Company
    { companyName
    , catchPhrase
    , bs
    } = AE.object
        [ "name" AE..= companyName
        , "catchPhrase" AE..= catchPhrase
        , "bs" AE..= bs
        ]

instance AE.FromJSON Company where
  parseJSON = AE.withObject "Company" $ \obj -> do
    companyName <- obj AE..: "name"
    catchPhrase <- obj AE..: "catchPhrase"
    bs <- obj AE..: "bs"
    return $ Company companyName catchPhrase bs


getUsers :: IO Users
getUsers = do
  res <- NW.asJSON =<< NW.get (baseURL ++ "/users")
  return $ res L.^. NW.responseBody

getUserById :: Int -> IO User
getUserById userId' = do
  res <- NW.asJSON =<< NW.get (baseURL ++ "/users/" ++ show userId')
  return $ res L.^. NW.responseBody

filterUsersByCity :: T.Text -> IO Users
filterUsersByCity city' = do
  users <- getUsers
  return $ filter ((== city') . city . address) users
