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
import Yesod.Form.Bootstrap5 (BootstrapFormLayout (..)
                             , renderBootstrap5
                             , BootstrapGridOptions(..)
                             )
import DoubleLayout
import Helper.Model
import Helper.Html

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
postAccessPointsR = do
  ((result, _), _) <- runFormPost (apForm Nothing)
  case result of
    FormSuccess ap -> do
      _ <- runDB $ insert ap
      setMessage "Access Point created."
      redirect AccessPointsR
    _ -> do
      doubleLayout [whamlet|<p>Something went wrong!
                           <p>#{show result}|]

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
postAccessPointR apId = do
  ((result, _), _) <- runFormPost (apForm Nothing)
  action <- lookupPostParam "action"
  case (result, action) of
    (FormSuccess ap, Just "save") -> do
      runDB $ replace apId ap
      setMessage "Updated Access Point."
      redirect $ AccessPointR apId
    (FormSuccess _, Just "delete") -> do
      runDB $ delete apId
      setMessage "Deleted Accesss Point."
      redirect AccessPointsR
    _ -> doubleLayout $ do
      setMessage "Error editing Access Point."
      redirect $ AccessPointR apId
    
apForm :: Maybe AccessPoint -> Form AccessPoint
apForm mAP = renderBootstrap5 bootstrapH $ AccessPoint
  <$> areq (selectField towerNames) towerSettings (accessPointTowerId <$> mAP)
  <*> areq (selectField aptNames) typeSettings (accessPointApTypeId <$> mAP)
  <*> areq textField nameSettings (accessPointName <$> mAP)
  <*> aopt intField heightSettings (accessPointHeight <$> mAP)
  <*> aopt intField azimuthSettings (accessPointAzimuth <$> mAP)
  <*> aopt doubleField tiltSettings (accessPointTilt <$> mAP)
  <*> aopt intField frequencySettings (accessPointFrequency <$> mAP)
  <*> aopt intField bandwidthSettings (accessPointChannelBandwidth <$> mAP)
  <*> aopt intField colorCodeSettings (accessPointColorCode <$> mAP)
  <*> aopt textField ssidSettings (accessPointSsid <$> mAP)
  <*> aopt textField ipSettings (accessPointIp <$> mAP)
  <*> aopt textField softwareSettings (accessPointSoftwareVersion <$> mAP)
  <*> aopt textAreaField backupSettings (accessPointBackupConfig <$> mAP)
  <*> areq dayField installSettings (accessPointInstallDate <$> mAP)
  <*> (getCurrentTime |> liftIO |> lift)
  <*> aopt textField macSettings (accessPointMac <$> mAP)
  <*> aopt textField msnSettings (accessPointMsn <$> mAP)
  where
    -- Which way to do this?
    towerNames = do
      towerEntities <- runDB $ selectList [] [Asc TowerName]
      optionsPairs $ map (\t -> (towerName $ entityVal t, entityKey t)) towerEntities
    -- aptNames = do
    --   aptEntities <- runDB $ getAccessPointTypeNames
    --   optionsPairs $ map (\(Value key, Value text) -> (text, key)) aptEntities
    aptNames = do
      aptEntities <- runDB $ selectList [] [Asc AccessPointTypeName]
      optionsPairs $ map (\apt -> (accessPointTypeName $ entityVal apt, entityKey apt)) aptEntities
    towerSettings = FieldSettings
      { fsLabel = "Tower"
      , fsTooltip = Nothing
      , fsId = Just "tower"
      , fsName = Just "tower"
      , fsAttrs =
        [ ("class", "form-select") ]
      }
    typeSettings = FieldSettings
      { fsLabel = "Type"
      , fsTooltip = Nothing
      , fsId = Just "type"
      , fsName = Just "type"
      , fsAttrs =
        [ ("class", "form-select") ]
      }
    nameSettings = FieldSettings
      { fsLabel = "Access Point Name"
      , fsTooltip = Nothing
      , fsId = Just "name"
      , fsName = Just "name"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "eg: bcn-3690-180")
        ]
      }
    heightSettings = FieldSettings
      { fsLabel = "Height"
      , fsTooltip = Nothing
      , fsId = Just "height"
      , fsName = Just "height"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "In whole number of feet, eg: 20")
        ]
      }
    azimuthSettings = FieldSettings
      { fsLabel = "Azimuth"
      , fsTooltip = Nothing
      , fsId = Just "azimuth"
      , fsName = Just "azimuth"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "In whole number of degrees, eg: 90")
        ]
      }
    tiltSettings = FieldSettings
      { fsLabel = "Tilt"
      , fsTooltip = Nothing
      , fsId = Just "tilt"
      , fsName = Just "tilt"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "In decimal number of degrees, eg: 1.25")
        ]
      }
    frequencySettings = FieldSettings
      { fsLabel = "Frequency"
      , fsTooltip = Nothing
      , fsId = Just "frequency"
      , fsName = Just "frequency"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "In whole number of megaherts, eg: 3590")
        ]
      }
    bandwidthSettings = FieldSettings
      { fsLabel = "Bandwidth"
      , fsTooltip = Nothing
      , fsId = Just "bandwidth"
      , fsName = Just "bandwidth"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "In whole number of megaherts, eg: 20")
        ]
      }
    colorCodeSettings = FieldSettings
      { fsLabel = "Color Code"
      , fsTooltip = Nothing
      , fsId = Just "colorCode"
      , fsName = Just "colorCode"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "An integer, eg: 5")
        ]
      }
    ssidSettings = FieldSettings
      { fsLabel = "SSID"
      , fsTooltip = Nothing
      , fsId = Just "ssid"
      , fsName = Just "ssid"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "SSID name, eg: bnc-south-ap")
        ]
      }
    ipSettings = FieldSettings
      { fsLabel = "IP"
      , fsTooltip = Nothing
      , fsId = Just "ip"
      , fsName = Just "ip"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "eg: 172.26.36.152")
        ]
      }
    softwareSettings = FieldSettings
      { fsLabel = "Software Version"
      , fsTooltip = Nothing
      , fsId = Just "software"
      , fsName = Just "software"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "eg: 20.3.1.1")
        ]
      }
    backupSettings = FieldSettings
      { fsLabel = "Backup File"
      , fsTooltip = Nothing
      , fsId = Just "backup"
      , fsName = Just "backup"
      , fsAttrs =
        [ ("class", "form-control") ]
      }
    installSettings = FieldSettings
      { fsLabel = "Install Date"
      , fsTooltip = Nothing
      , fsId = Just "install"
      , fsName = Just "install"
      , fsAttrs =
        [ ("class", "form-control") ]
      }
    macSettings = FieldSettings
      { fsLabel = "MAC"
      , fsTooltip = Nothing
      , fsId = Just "mac"
      , fsName = Just "mac"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "eg: 0a-00-3e-60-7d-be")
        ]
      }
    msnSettings = FieldSettings
      { fsLabel = "Serial Number"
      , fsTooltip = Nothing
      , fsId = Just "msn"
      , fsName = Just "msn"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "eg: M9WK00X4RCK2")
        ]
      }
