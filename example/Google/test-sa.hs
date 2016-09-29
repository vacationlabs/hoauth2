{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-

-}

module Main where

import           Keys                          (googleServiceAccount)
import          qualified Network.OAuth.OAuth2.ServiceAccount as SA

import           Control.Monad                 (liftM)
import           Data.Aeson                    (FromJSON)
import           Data.Aeson.TH                 (defaultOptions, deriveJSON)
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Internal as BL
import           Data.Text                     (Text)
import           Network.HTTP.Conduit
import           Prelude                       hiding (id)
import           System.Environment            (getArgs)

--------------------------------------------------

data Token = Token { issued_to   :: Text
                   , audience    :: Text
                   , user_id     :: Maybe Text
                   , scope       :: Text
                   , expires_in  :: Integer
                   , access_type :: Text
                   } deriving (Show)


$(deriveJSON defaultOptions ''Token)

data User = User { id          :: Text
                 , name        :: Text
                 , given_name  :: Text
                 , family_name :: Text
                 , link        :: Text
                 , picture     :: Text
                 , gender      :: Text
                 , locale      :: Text
                 } deriving (Show)

$(deriveJSON defaultOptions ''User)

--------------------------------------------------

main :: IO ()
main = do
    xs <- getArgs
    mgr <- newManager tlsManagerSettings
    case xs of
        ["offline"] -> offlineCase mgr
        _ -> normalCase mgr

offlineCase :: Manager -> IO ()
offlineCase mgr = do
  (authUri, body) <- SA.saAccessTokenUrl googleServiceAccount
  print authUri
  print body
