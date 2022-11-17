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
{-# LANGUAGE NamedFieldPuns #-}
module Handler.Home where
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Data.Aeson
import Debug.Trace
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

getSessionFromPin :: Import.Text -> DB (Maybe (Entity Session))
getSessionFromPin pin = selectFirst [SessionPinCode ==. pin] []

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

updateSession :: Int -> PlexUserResponse -> Handler ()
updateSession ssnId userResponse = do
    liftIO $ traceIO "[updateSession] update session"
    let authToken = plexUserResponseAuthToken userResponse
        username = plexUserResponseUsername userResponse
        title = plexUserResponseTitle userResponse
        email = plexUserResponseEmail userResponse
    runDB $ updateWhere [SessionPinId ==. ssnId] [
            SessionAuthToken =. Just authToken,
            SessionUsername =. Just username,
            SessionTitle =. Just title,
            SessionEmail =. Just email
        ]

fetchPlexPin :: IO (Either String PlexPinResponse)
fetchPlexPin = do
    traceIO "[fetchPlexPin] Generate new HTTP manager"
    manager <- Network.HTTP.Client.newManager tlsManagerSettings
    let headers = [("Content-Type", "application/x-www-form-urlencoded"), ("accept", "application/json")]
        body = [partBS "strong" "true", partBS "X-Plex-Product" (lazyTextToByteString plexProduct), partBS "X-Plex-Client-Identifier" (lazyTextToByteString plexIdentifier)]
    traceIO "[fetchPlexPin] Make HTTP request to Plex"
    (plexResponse, status) <- postFormData manager "https://plex.tv/api/v2/pins" headers body
    traceIO ("[fetchPlexPin] Got a " ++ show status ++ " response")
    traceIO (show plexResponse)
    case status of
        200 -> pure $ eitherDecode plexResponse
        201 -> pure $ eitherDecode plexResponse
        _ -> pure $ Left "Improper response from plex"

fetchPlexUser :: Maybe Import.Text -> IO (Either String PlexUserResponse)
fetchPlexUser authToken = case authToken of
    Nothing -> pure $ Left "[fetchPlexUser]: No auth token provided"
    Just tkn -> do
        traceIO "[fetchPlexUser] Generate new HTTP manager"
        manager <- Network.HTTP.Client.newManager tlsManagerSettings
        traceIO "[fetchPlexUser] Make HTTP request to Plex"
        let strIdent = TL.unpack plexIdentifier
            strProduct = TL.unpack plexProduct
            strToken = Import.unpack tkn
            url = "https://plex.tv/api/v2/user/?X-Plex-Product" ++ strProduct ++ "&X-Plex-Client-Identifier=" ++ strIdent ++ "&X-Plex-Token=" ++ strToken
            headers = [("accept", "application/json")]
        traceIO $ "[fetchPlexUser] auth token: " ++ strToken
        traceIO $ "[fetchPlexUser] product: " ++ strProduct
        traceIO $ "[fetchPlexUser] identifier: " ++ strIdent
        traceIO $ "[fetchPlexUser] url: " ++ url
        (plexResponse, status) <- fetch manager url headers
        case status of
            200 -> pure $ eitherDecode plexResponse
            _ -> pure $ Left "Improper response from plex"

followPlexPin :: Int -> Import.Text -> IO (Either String PlexUserResponse)
followPlexPin ssnId code = do
    traceIO "[fetchPlexPin] Generate new HTTP manager"
    manager <- Network.HTTP.Client.newManager tlsManagerSettings
    traceIO "[fetchPlexPin] Make HTTP request to Plex"
    let strCode = Import.unpack code
        strIdent = TL.unpack plexIdentifier
        url = "https://plex.tv/api/v2/pins/" ++ show ssnId ++ "?code=" ++ strCode ++ "&X-Plex-Client-Identifier=" ++ strIdent
        headers = [("accept", "application/json")]
    traceIO $ "[fetchPlexPin] pin code: " ++ strCode
    traceIO $ "[fetchPlexPin] identifier: " ++ strIdent
    traceIO $ "[fetchPlexPin] url: " ++ url
    (plexResponse, status) <- fetch manager url headers
    traceIO ("[fetchPlexPin] Got a " ++ show status ++ " response")
    traceIO (show plexResponse)
    case status of
        200 -> case eitherDecode plexResponse of
            Left parseErr -> pure $ Left parseErr
            Right pinResponse -> do
                let authToken = plexPinResponseAuthToken pinResponse
                fetchPlexUser authToken
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

getFollowUpPlexAuthR :: Handler Value
getFollowUpPlexAuthR = do
    maybePin <- lookupGetParam "pin"
    case maybePin of
        Nothing -> sendResponseStatus status400 ("[getFollowUpPlexAuthR] You must provide the pin you are trying to follow up on" :: Import.Text)
        Just pin -> do
            maybeSession <- runDB $ getSessionFromPin pin
            case maybeSession of
                Nothing -> sendResponseStatus status400 ("[getFollowUpPlexAuthR] No session matching that pin":: Import.Text)
                Just sessionEnt -> do
                    let session = entityVal sessionEnt
                        ssnId = sessionPinId session
                        ssnPin = sessionPinCode session
                        maybeAuthToken = sessionAuthToken session
                    plexUserResponse <- liftIO $ fetchPlexUser maybeAuthToken
                    case plexUserResponse of
                        Left _ -> do
                            plexPinResponse <- liftIO $ followPlexPin ssnId ssnPin
                            case plexPinResponse of
                                Left _ -> liftIO $ traceIO "[getFollowUpPlexAuthR] Could not update session information"
                                Right usr -> updateSession ssnId usr
                            returnJson plexPinResponse
                        Right user -> returnJson user


