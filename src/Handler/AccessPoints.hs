{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Handler.AccessPoints where

import Import hiding (Value)
import Yesod.Form.Bootstrap5 (BootstrapFormLayout (..)
                             , renderBootstrap5
                             , BootstrapGridOptions(..)
                             )
import DoubleLayout
import Helper.Html

import Database.Esqueleto.Experimental as E hiding(delete, isNothing, (==.))
import Data.Conduit.Binary
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Handler.AccessPoints.AccessPointTypes hiding(fileInfo)
import Handler.AccessPoints.Comments
import Handler.Backups
import Data.Conduit.Binary ( sinkLbs )
import Database.Persist.Sql ( rawExecute ) 

getAllAccessPoints :: DB [Entity AccessPoint]
getAllAccessPoints = selectList [] [Asc AccessPointName]

getAccessPointsR :: Handler Html
getAccessPointsR = do
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

apsTableWidget :: Widget
apsTableWidget = do
  allAPs <- handlerToWidget $ runDB getAllAccessPoints
  (widget, enctype) <- handlerToWidget $ generateFormPost (apForm Nothing)
  $(widgetFile "accessPoints/accessPointsTable")

editAPWidget :: Maybe (AccessPointId, AccessPoint) -> Widget
editAPWidget mp = do
  let (actionR, mAP) = case mp of
        Nothing -> (AccessPointsR, Nothing)
        Just (apId, ap) -> (AccessPointR apId, Just ap)
  (widget, enctype) <- handlerToWidget $ generateFormPost (apForm mAP)
  $(widgetFile "accessPoints/accessPointEdit")

postAccessPointsR :: Handler Html
postAccessPointsR = do
  ((result, _), _) <- runFormPost $ apForm Nothing
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
  apBackups <- runDB $ getAPBackupsFor apId
  let
    backupPostR = APBackupsR apId
    backupDiscardR = AccessPointR apId
    filestores = map toFileStore apBackups
  doubleLayout $ do
    setTitle "Access Point"
    $(widgetFile "accessPoints/accessPoint")
  where
    apTableWidget ap = do
      let
        tId = accessPointTowerId ap
        aptId = accessPointApTypeId ap
      tower <-  handlerToWidget $ runDB $ get404 tId
      apType <- handlerToWidget $ runDB $ get404 aptId
      (widget, enctype) <- handlerToWidget $ generateFormPost (apForm (Just ap))
      $(widgetFile "accessPoints/accessPointTable")
    toFileStore (Entity apKey ap) = Entity fsKey fsVal
      where
        fsKey = accessPointBackupFilestoreId ap
        fsVal = FileStore
          { fileStoreFilename = accessPointBackupFilename ap
          , fileStoreContentType = accessPointBackupContentType ap
          , fileStorePayload = accessPointBackupPayload ap
          , fileStoreUpdatedAt = accessPointBackupUpdatedAt ap
          }

postAccessPointR :: AccessPointId -> Handler Html
postAccessPointR apId = do
  ((result, _), _) <- runFormPost $ apForm Nothing
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
apForm mAP = apTowerForm Nothing mAP

apTowerForm :: Maybe TowerId -> Maybe AccessPoint -> Form AccessPoint
apTowerForm mTID mAP = renderBootstrap5 bootstrapH $ AccessPoint
  <$> areq (selectField towerNames) towerSettings (chooseTowerId mTID mAP)
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
    chooseTowerId :: Maybe TowerId -> Maybe AccessPoint -> Maybe TowerId
    chooseTowerId Nothing Nothing = Nothing
    chooseTowerId Nothing (Just ap) = Just (accessPointTowerId ap)
    chooseTowerId (Just tid) Nothing = Just tid
    chooseTowerId (Just tid) (Just ap) = Just (accessPointTowerId ap)
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

getAllAPBackups :: AccessPointId -> DB [(Single FileStoreId,Single UTCTime,Single Text)]
getAllAPBackups apId = rawSql "SELECT backupid,updated_at,filename FROM aps_backups WHERE apid=? ORDER BY updated_at DESC" [PersistText (keyToText apId)]

getAPBackupsFor :: AccessPointId -> DB [Entity AccessPointBackup]
getAPBackupsFor apId = selectList [AccessPointBackupAccessPointId ==. apId]
                                  [Desc AccessPointBackupUpdatedAt]

postAPBackupsR :: AccessPointId -> Handler Html
postAPBackupsR apId = do
  ((result, _), _) <- runFormPost backupForm
  case result of
    FormSuccess b -> do
      --TODO: Clean this up
      bytes <- connect (b |> fileInfo |> fileSource) sinkLbs
      let
        myFN = fileName $ fileInfo b
        myUpdatedAt = createdAt b
        myContentType = fileContentType $ fileInfo b
      bId <- runDB $ insert $ FileStore myFN myContentType (bytes |> toStrict) myUpdatedAt
      _ <- runDB $ rawExecute "insert into accesspoint_backup (accesspointid, backupid) values (?,?)"
                     [PersistText (keyToText apId), PersistText (keyToText bId)]
      setMessage "Backup saved."
      redirect $ AccessPointR apId
    _ -> doubleLayout [whamlet|Someting went wrong!|]
  
