{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TypeFamilies         #-}
module Handler.Home where
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.Wai (responseLBS)
import Data.Aeson (decode)

import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    aDomId <- newIdent
    setTitle "Plex Content Request"
    $(widgetFile "homepage")

getAllRequests :: DB [Entity ContentRequest]
getAllRequests = selectList [] [Asc ContentRequestCreatedAt]

getRequestsR :: Handler Value
getRequestsR = do
    requests <- runDB getAllRequests
    returnJson (Import.map entityVal requests)

postCreateRequestR :: Handler Value
postCreateRequestR = do
    request <- (requireCheckJsonBody :: Handler ContentRequest)
    insertedRqst <- runDB $ insertEntity request
    returnJson insertedRqst

getStartPlexAuthR :: IO (Handler Value)
getStartPlexAuthR = do
    manager <- Network.HTTP.Client.newManager tlsManagerSettings
    print $ stringToByteString "Test"
    let headers = [("Content-Type", "application/json")]
    let body = object [("strong" .= "true"), ("X-Plex-Product" .= plexProduct), ("X-Plex-Client-Identifier" .= plexIdentifier)]
    (plexResponse, status) <- post manager "https://plex.tv/api/v2/user" headers body
    case status of
        200 -> do
            let plexPin = decode plexResponse :: Maybe PlexPinResponse
            case plexPin of
                Just pin -> pure $ sendResponse $ generatePlexAuthURL $ plexPinResponseCode pin
                Nothing -> pure $ sendResponseStatus status500 ("There was an issue generating the Plex Auth URL" :: Text)
        _ -> pure $ sendResponseStatus status500 ("There was an issue generating the Plex Auth URL" :: Text)
    
    