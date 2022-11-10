{-# LANGUAGE TemplateHaskell #-}
module Lib (fetch, post, plexProduct, plexIdentifier, generatePlexAuthURL, stringToByteString, PlexPinResponse (..)) where
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Data.Char (toLower)
import Network.HTTP.Types.Header
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString            as B
import Data.ByteString.Lazy       as BL
import Data.Text.Lazy             as TL
import Data.Text.Lazy.Encoding    as TL
import Data.Text.Lazy.IO          as TL

data PlexPinResponse = PlexPinResponse {
        plexPinResponseId :: Int,
        plexPinResponseCode :: String,
        plexPinResponseProduct :: String,
        plexPinResponseTrusted :: Bool,
        plexPinResponseQr :: String,
        plexPinResponseClientIdentifier :: String,
        plexPinResponseExpiresIn :: Int
    } deriving (Show)

$(deriveJSON (defaultOptions{fieldLabelModifier = Prelude.drop 15, constructorTagModifier = Prelude.map Data.Char.toLower}) ''PlexPinResponse)

plexProduct :: String
plexProduct = "Kev Plex Request App"

plexIdentifier :: String
plexIdentifier = "kevf050b022-aba2-43ed-93d7-26b768094023request"

generatePlexAuthURL :: String -> String
generatePlexAuthURL pin = "https://app.plex.tv/auth#?clientID=" ++ plexIdentifier ++ "&code=" ++ pin ++ "&context%5Bdevice%5D%5Bproduct%5D=Kev%20Plex%20Request%20App&forwardUrl=http://localhost:3006"

stringToByteString :: String -> BL.ByteString
stringToByteString = TL.encodeUtf8 . TL.pack

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