{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RankNTypes #-}
module Handler.Home where
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.Wai (responseLBS)
import Data.Aeson
import Debug.Trace
import Data.Either
import Network.HTTP.Client.MultipartFormData
import Data.Text.Lazy    as TL
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
    _ <- newIdent
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

createSession :: PlexPinResponse -> HandlerFor App PlexPinResponse
createSession pin = do
    _ <- runDB $ insertEntity Session {
        sessionPinId = plexPinResponseId pin,
        sessionPinCode = TL.toStrict $ plexPinResponseCode pin,
        sessionAuthToken = Nothing,
        sessionUsername = Nothing,
        sessionTitle = Nothing,
        sessionEmail = Nothing
    }
    pure pin

fetchPlexPin :: IO (Either String PlexPinResponse)
fetchPlexPin = do
    traceIO "[fetchPlexPin] Generate new HTTP manager"
    manager <- Network.HTTP.Client.newManager tlsManagerSettings
    let headers = [("Content-Type", "application/x-www-form-urlencoded"), ("accept", "application/json")]
    let body = [partBS "strong" "true", partBS "X-Plex-Product" (lazyTextToByteString plexProduct), partBS "X-Plex-Client-Identifier" (lazyTextToByteString plexIdentifier)]
    traceIO "[fetchPlexPin] Make HTTP request to Plex"
    (plexResponse, status) <- postFormData manager "https://plex.tv/api/v2/pins" headers body
    traceIO ("[fetchPlexPin] Got a " ++ show status ++ " response")
    traceIO (show plexResponse)
    case status of
        200 -> pure $ eitherDecode plexResponse
        201 -> pure $ eitherDecode plexResponse
        _ -> pure $ Left "Improper response from plex"

getStartPlexAuthR :: Handler Value
getStartPlexAuthR = do
    plexPinResponse <- liftIO fetchPlexPin
    liftIO $ traceIO (show plexPinResponse)
    case plexPinResponse of
        Left _ -> sendResponseStatus status500 ("[getStartPlexAuthR] There was an issue generating the Plex Auth URL":: Import.Text)
        Right pin -> do
            _ <- createSession pin
            sendResponse $ generatePlexAuthURL $ plexPinResponseCode pin

getSessionFromPin :: Import.Text -> DB (Maybe (Entity Session))
getSessionFromPin pin = selectFirst [SessionPinCode ==. pin] []

followPlexPin :: Int -> Import.Text -> IO (Either String PlexPinResponse)
followPlexPin ssnId code = do
    traceIO "[fetchPlexPin] Generate new HTTP manager"
    manager <- Network.HTTP.Client.newManager tlsManagerSettings
    traceIO "[fetchPlexPin] Make HTTP request to Plex"
    let strCode = Import.unpack code
    let strIdent = TL.unpack plexIdentifier
    let headers = [("accept", "application/json")]
    (plexResponse, status) <- fetch manager ("https://plex.tv/api/v2/pins/" ++ show ssnId ++ "?code=" ++ strCode ++ "X-Plex-Identifier" ++ strIdent) headers
    traceIO ("[fetchPlexPin] Got a " ++ show status ++ " response")
    traceIO (show plexResponse)
    case status of
        200 -> pure $ eitherDecode plexResponse
        201 -> pure $ eitherDecode plexResponse
        _ -> pure $ Left "Improper response from plex"

getFollowUpPlexAuthR :: Handler Value
getFollowUpPlexAuthR = do
    maybePin <- lookupGetParam "pin"
    case maybePin of
        Nothing -> sendResponseStatus status400 ("[getFollowUpPlexAuthR] You must provide the pin you are trying to follow up on":: Import.Text)
        Just pin -> do
            maybeSession <- getSessionFromPin pin -- How to fix this monad context error
            case maybeSession of
                Nothing -> sendResponseStatus status400 ("[getFollowUpPlexAuthR] No session matching that pin":: Import.Text)
                Just sessionEnt -> do
                    let session = entityVal sessionEnt -- Can I pattern match this in the case instead of needing to call this function?
                    let ssnId = sessionPinId session
                    let ssnPin = sessionPinCode session
                    plexPinResponse <- liftIO $ followPlexPin ssnId ssnPin
                    returnJson plexPinResponse


