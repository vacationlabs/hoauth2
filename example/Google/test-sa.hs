{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-

-}

module Main where

import           Keys                          (googleServiceAccount)
import          Network.OAuth.OAuth2

import           Data.Aeson                    (FromJSON)
import           Control.Monad                 (liftM)
import           Data.Aeson.TH                 (defaultOptions, deriveJSON)
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import           Data.Text                     (Text)
import           Network.HTTP.Conduit
import           Prelude                       hiding (id)
import           System.Environment            (getArgs)

--------------------------------------------------

main :: IO ()
main = do
    xs <- getArgs
    mgr <- newManager tlsManagerSettings
    case xs of
        ["offline"] -> offlineCase mgr
        _ -> normalCase mgr

offlineCase :: Manager -> IO ()
offlineCase = undefined

normalCase :: Manager -> IO ()
normalCase mgr = do
  (authUri, body) <- saAccessTokenUrl googleServiceAccount
  print authUri
  print body
  result <-fetchAT mgr authUri body
  case result of
    Right token -> (fileAPI mgr token) >>= print
    Left e -> print e

fetchAT :: Manager -> URI -> PostBody -> IO (OAuth2Result AccessToken)
fetchAT mgr uri body = liftM parseResponseJSON (makeReq mgr uri body)

makeReq :: Manager -> URI -> PostBody -> IO (OAuth2Result BSL.ByteString)
makeReq mgr uri body = liftM handleResponse go
  where go = do
          req <- parseUrl $ BS.unpack uri
          httpLbs (urlEncodedBody body req) mgr

fileAPI :: Manager
           -> AccessToken
           -> IO (OAuth2Result BSL.ByteString)
fileAPI mgr token = authGetBS mgr token "https://www.googleapis.com/drive/v2/files"
