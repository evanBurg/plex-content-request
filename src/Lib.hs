{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib (fetch, post, postFormData, plexProduct, plexIdentifier, generatePlexAuthURL, stringToByteString, lazyTextToByteString, PlexPinResponse (..)) where
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.Char (toLower)
import Network.HTTP.Types.Header
import Data.Aeson
import Data.ByteString            as B
import Data.ByteString.Lazy       as BL
import Data.Text.Lazy             as TL
import Data.Text.Lazy.Encoding    as TL
import Data.Text.Lazy.IO          as TL
import Data.Text                  as T
import Data.Text.Encoding         as T
import Data.Text.IO               as T
import Network.HTTP.Client.MultipartFormData
import Debug.Trace

-- data PlexLocation = PlexLocation {
--     plexLocationCode :: String,
--     plexLocationEuropeanUnionMember :: Bool,
--     plexLocationContinentCode :: String,
--     plexLocationCountry :: String,
--     plexLocationTimeZone :: String,
--     plexLocationPostalCode :: String,
--     plexLocationInPrivacyRestrictedCountry :: Bool,
--     plexLocationSubdivisions :: String,
--     plexLocationCoordinates :: String,
-- }

data PlexPinResponse = PlexPinResponse {
        plexPinResponseId :: Int,
        plexPinResponseCode :: TL.Text,
        plexPinResponseProduct :: TL.Text,
        plexPinResponseTrusted :: Bool,
        plexPinResponseQr :: TL.Text,
        plexPinResponseClientIdentifier :: TL.Text,
        -- plexPinResponseLocation :: PlexLocation,
        plexPinResponseExpiresIn :: Int
        -- plexPinResponseCreatedAt :: Int,
        -- plexPinResponseExpiresAt :: Int,
        -- plexPinResponseAuthToken :: Maybe Bool,
        -- plexPinResponseNewRegistration :: Maybe Bool,
    } deriving (Show)

instance FromJSON PlexPinResponse where
    parseJSON = traceStack "Original" $ withObject "PlexPinResponse" $ \v -> PlexPinResponse
        <$> v .: "id"
        <*> v .: "code"
        <*> v .: "product"
        <*> v .: "trusted"
        <*> v .: "qr"
        <*> v .: "clientIdentifier"
        <*> v .: "expiresIn"

-- $(deriveJSON (defaultOptions{fieldLabelModifier = Prelude.drop 15, constructorTagModifier = Prelude.map Data.Char.toLower}) ''PlexPinResponse)

plexProduct :: TL.Text
plexProduct = "Kev Plex Request App"

plexIdentifier :: TL.Text
plexIdentifier = "kevf050b022-aba2-43ed-93d7-26b768094023request"

generatePlexAuthURL :: TL.Text -> TL.Text
generatePlexAuthURL pin = "https://app.plex.tv/auth#?clientID=" <> plexIdentifier <> "&code=" <> pin <> "&context%5Bdevice%5D%5Bproduct%5D=Kev%20Plex%20Request%20App&forwardUrl=http%3A%2F%2Flocalhost%3A5173%2F%3FfollowUpPin%3D" <> pin

stringToByteString :: String -> BL.ByteString
stringToByteString = TL.encodeUtf8 . TL.pack

lazyTextToByteString :: TL.Text -> B.ByteString
lazyTextToByteString = T.encodeUtf8 . TL.toStrict 

fetch :: Manager -> String -> RequestHeaders -> IO (BL.ByteString, Int)
fetch manager url headers = do
    initialRequest <- parseRequest url
    let request = initialRequest { requestHeaders = headers }
    resp <- httpLbs request manager
    let status = statusCode $ responseStatus resp
    pure (responseBody resp, status)

post :: (ToJSON a) => Manager -> String -> RequestHeaders -> a -> IO (BL.ByteString, Int)
post manager url headers body = do
    initialRequest <- parseRequest $ "POST " ++ url
    let request = initialRequest { requestBody = RequestBodyLBS $ encode body, requestHeaders = headers }
    resp <- httpLbs request manager
    let status = statusCode $ responseStatus resp
    pure (responseBody resp, status)

postFormData :: Manager -> String -> RequestHeaders -> [Part] -> IO (BL.ByteString, Int)
postFormData manager url headers body = do
    initialRequest <- parseRequest $ "POST " ++ url
    let request = initialRequest { requestHeaders = headers }
    newReq <- formDataBody body request
    resp <- httpLbs newReq manager
    let status = statusCode $ responseStatus resp
    pure (responseBody resp, status)