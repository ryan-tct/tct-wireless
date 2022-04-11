{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module Handler.AccessPoints where

import Import hiding (Value)
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)
import Database.Esqueleto.Experimental as E
import DoubleLayout

getAllAccessPoints :: DB [Entity AccessPoint]
getAllAccessPoints = selectList [] [Asc AccessPointName]

getAccessPointsR :: Handler Html
getAccessPointsR = do
  allAPs <- runDB getAllAccessPoints
  (widget, enctype) <- generateFormPost (apForm Nothing)
  doubleLayout $ do
    setTitle "Access Points"
    $(widgetFile "accessPoints/accessPoints")
    
getAPRow :: Entity AccessPoint -> Widget
getAPRow (Entity apId ap) = do
  let tId = accessPointTowerId ap
  let aptId = accessPointApTypeId ap
  mtower <- handlerToWidget $ runDB $ get tId
  mapType <- handlerToWidget $ runDB $ get aptId
  $(widgetFile "accessPoints/accessPointRow")

postAccessPointsR :: Handler Html
postAccessPointsR = undefined

getAccessPointR :: AccessPointId -> Handler Html
getAccessPointR apId = do
  ap <- runDB $ get404 apId
  let tId = accessPointTowerId ap
  let aptId = accessPointApTypeId ap
  tower <-  runDB $ get404 tId
  apType <- runDB $ get404 aptId
  (widget, enctype) <- generateFormPost (apForm (Just ap))
  doubleLayout $ do
    setTitle "Access Point"
    $(widgetFile "accessPoints/accessPoint")


postAccessPointR :: AccessPointId -> Handler Html
postAccessPointR apId = undefined

apForm :: Maybe AccessPoint -> Form AccessPoint
apForm mAP = renderBootstrap4 BootstrapBasicForm $ AccessPoint
  <$> pure (toSqlKey 1000) -- areq textField "towerid" (accessPointTowerId mAP)
  <*> pure (toSqlKey 1000) -- areq textField "aptid" (accessPointApTypeId mAP)
  <*> areq textField "name" (accessPointName <$> mAP)
  <*> aopt intField "height" (accessPointHeight <$> mAP)
  <*> aopt intField "azimuth" (accessPointAzimuth <$> mAP)
  <*> aopt doubleField "tilt" (accessPointTilt <$> mAP)
  <*> aopt intField "frequency" (accessPointFrequency <$> mAP)
  <*> aopt intField "bandwidth" (accessPointChannelBandwidth <$> mAP)
  <*> aopt intField "colorcode" (accessPointColorCode <$> mAP)
  <*> aopt textField "ssid" (accessPointSsid <$> mAP)
  <*> aopt textField "ip" (accessPointIp <$> mAP)
  <*> aopt textAreaField "software" (accessPointSoftwareVersion <$> mAP)
  <*> aopt textAreaField "backup" (accessPointBackupConfig <$> mAP)
  <*> areq dayField "install" (accessPointInstallDate <$> mAP)
  <*> (getCurrentTime |> liftIO |> lift)
  <*> aopt textField "mac" (accessPointMac <$> mAP)
  <*> aopt textField "msn" (accessPointMsn <$> mAP)
  where
    textAreaField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Text
    textAreaField = Field
      { fieldParse = parseHelper $ Right
      , fieldView = \theId fname attrs val isReq ->
          [whamlet|
$newline never
<textarea id="#{theId}" name="#{fname}" *{attrs} type="text" :isReq:required>#{either id id val}
          |]
      , fieldEnctype = UrlEncoded
      }
