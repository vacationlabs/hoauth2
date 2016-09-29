{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_HADDOCK -ignore-exports #-}

-- | Google Service Account
-- https://developers.google.com/identity/protocols/OAuth2ServiceAccount

module Network.OAuth.OAuth2.GoogleServiceAccount where

import qualified Network.Google.OAuth2.JWT as JWT
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Text (Text)
import qualified Data.Text.Encoding as T

import Network.OAuth.OAuth2.Internal

data GoogleServiceAccount = GoogleServiceAccount
  { saPrivateKey    :: BS.ByteString       -- ^ Private Key
  , saEmail         :: BS.ByteString       -- ^ client email
  , saScope         :: [ BS.ByteString ]   -- ^ list of the permissions that the app requests.
  , saAuthEndpoint  :: BS.ByteString       -- ^ access token request endpoint
  , saExpireTime    :: Maybe Int           -- ^ expiration time of the assertion, max 1 hour
  , saDelegateEmail :: Maybe BS.ByteString -- ^ email address of the user for which the application is requesting delegated access.
  }

saGrantType :: BS.ByteString
saGrantType = "urn:ietf:params:oauth:grant-type:jwt-bearer"


--
saAccessTokenUrl :: GoogleServiceAccount -> IO (URI, PostBody)
saAccessTokenUrl sa = do
  jwtString <- jwt sa
  case jwtString of
    Left e -> error e
    Right assertion -> return (saAuthEndpoint sa
                             , transform' [ ("grant_type", Just saGrantType)
                                          , ("assertion", Just assertion)
                                          ]
                             )

jwt :: GoogleServiceAccount
   -> IO (Either String BS.ByteString) -- ^ JWT assertion
jwt sa = privateKey >>= JWT.getSignedJWT email delegateEmail scope expire
  where email = bsToText $ saEmail sa
        delegateEmail = fmap bsToText (saDelegateEmail sa)
        scope = map bsToText (saScope sa)
        expire = saExpireTime sa
        privateKey = JWT.fromPEMString $ BS8.unpack $ saPrivateKey sa


bsToText :: BS.ByteString -> Text
bsToText = T.decodeUtf8
