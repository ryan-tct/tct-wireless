{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module Handler.Equipment where

import Import hiding (Value)
import Yesod.Form.Bootstrap5 (BootstrapFormLayout (..)
                             , renderBootstrap5
                             , BootstrapGridOptions(..)
                             )
import Helper.Model hiding ((==.))
import Helper.Html
import DoubleLayout
import Handler.Equipment.EquipmentTypes

getAllEquipment :: DB [Entity Equipment]
getAllEquipment = selectList [] [Desc EquipmentName]

getEquipmentOn :: TowerId -> DB [Entity Equipment]
getEquipmentOn tId = selectList [EquipmentTowerId ==. tId] [Desc EquipmentUpdatedAt]

getAllEqBackupsFor :: EquipmentId -> DB [Entity EquipmentBackup]
getAllEqBackupsFor eId = selectList [EquipmentBackupEquipmentId ==. eId]
                                    [Desc EquipmentBackupUpdatedAt]

equipmentsListWidget :: Foldable t => t (Entity Equipment) -> Widget
equipmentsListWidget allEquipment = $(widgetFile "equipment/equipmentsList")

getEquipmentsR :: Handler Html
getEquipmentsR = do
  allEquipment <- runDB getAllEquipment
  allEquipmentTypes <- runDB getAllEquipmentTypes
  let
    met = Nothing
    postETR = EquipmentTypesR
    discardETR = EquipmentsR
    me = Nothing
    postR = EquipmentsR
    discardR = EquipmentsR
  doubleLayout $ do
    setTitle "Equipment"
    $(widgetFile "equipment/equipmentsView")

postEquipmentsR :: Handler Html
postEquipmentsR = do
  ((result, _), _) <- runFormPost $ equipmentForm Nothing
  case result of
    FormSuccess e -> do
      runDB $ insert e
      setMessage "Equipment created."
      redirect EquipmentsR
    _ -> do
      doubleLayout [whamlet|<p>Something went wrong!
                            <p>#{show result}
                           |]

getEquipmentR :: EquipmentId -> Handler Html
getEquipmentR eId = do
  e <- runDB $ get404 eId
  let
    me = Just e
    postR = EquipmentR eId
    discardR = EquipmentR eId
    etId = equipmentEquipmentTypeId e
    tId = equipmentTowerId e
  et <- runDB $ get404 etId
  t <- runDB $ get404 tId
  etDocs <- runDB $ getEqTypeDocsFor etId
  eBackups <- runDB $ getAllEqBackupsFor eId
  let filestores = map toFileStore eBackups
  doubleLayout $ do
    setTitle "Equipment"
    $(widgetFile "equipment/equipmentView")
  where
    toFileStore (Entity ebKey eb) = Entity fsKey fsVal
      where
        fsKey = equipmentBackupFilestoreId eb
        fsVal = FileStore
          { fileStoreFilename = equipmentBackupFilename eb
          , fileStoreContentType = equipmentBackupContentType eb
          , fileStorePayload = equipmentBackupPayload eb
          , fileStoreUpdatedAt = equipmentBackupUpdatedAt eb
          }

postEquipmentR :: EquipmentId -> Handler Html
postEquipmentR eId = do
  ((result, _), _) <- runFormPost $ equipmentForm Nothing
  action <- lookupPostParam "action"
  case (result, action) of
    (FormSuccess e, Just "save") -> do
      runDB $ replace eId e
      setMessage "Updated equipment."
      redirect $ EquipmentR eId
    (FormSuccess _, Just "delete") -> do
      runDB $ delete eId
      setMessage "Deleted equipment."
      redirect EquipmentsR
    _ -> doubleLayout $ do
      setMessage "Error editing equipment."
      redirect $ EquipmentR eId

fileStoreWidget filestores = $(widgetFile "filestores/filestoreTable")

equipmentTableWidget :: EquipmentId
                     -> Equipment
                     -> EquipmentTypeId
                     -> EquipmentType
                     -> TowerId
                     -> Tower
                     -> Widget
equipmentTableWidget eId e etId et tId t = $(widgetFile "equipment/equipmentTable")

getEquipmentRow :: Entity Equipment -> Widget
getEquipmentRow (Entity eId e) = do
  let
    tId = equipmentTowerId e
    etId = equipmentEquipmentTypeId e
  mtower <- handlerToWidget $ runDB $ get tId
  mequipmentType <- handlerToWidget $ runDB $ get etId
  $(widgetFile "equipment/equipmentRow")

equipmentFormWidget :: Maybe Equipment
                    -> Route App
                    -> Route App
                    -> Widget
equipmentFormWidget me postR discR = equipmentTowerFormWidget Nothing me postR discR

equipmentTowerFormWidget :: Maybe TowerId
                         -> Maybe Equipment
                         -> Route App
                         -> Route App
                         -> Widget
equipmentTowerFormWidget mtId me postR discardR = do
  (formWidget, formEncType) <- handlerToWidget $ generateFormPost $ equipmentTowerForm mtId me
  $(widgetFile "forms/generalForm")
  
equipmentForm :: Maybe Equipment -> Form Equipment
equipmentForm me = equipmentTowerForm Nothing me

equipmentTowerForm :: Maybe TowerId -> Maybe Equipment -> Form Equipment
equipmentTowerForm mtId me = renderBootstrap5 bootstrapH $ Equipment
  <$> areq (selectField towerNames) towerSettings (chooseTowerId mtId me)
  <*> areq textField nameSettings (equipmentName <$> me)
  <*> areq (selectField etNames) typeSettings (equipmentEquipmentTypeId <$> me)
  <*> aopt textField ipSettings (equipmentIp <$> me)
  <*> aopt textField macSettings (equipmentMac <$> me)
  <*> aopt textField msnSettings (equipmentMsn <$> me)
  <*> aopt textField softwareVersionSettings (equipmentSoftwareVersion <$> me)
  <*> areq dayField createdAtSettings (equipmentCreatedAt <$> me)
  <*> (getCurrentTime |> liftIO |> lift)
  where
    towerNames = do
      towerEntities <- runDB $ selectList [] [Asc TowerName]
      optionsPairs $ map (\t -> (towerName $ entityVal t, entityKey t)) towerEntities
    etNames = do
      etEntities <- runDB $ selectList [] [Asc EquipmentTypeName]
      optionsPairs $ map (\et -> (equipmentTypeName $ entityVal et, entityKey et)) etEntities
    chooseTowerId :: Maybe TowerId -> Maybe Equipment -> Maybe TowerId
    chooseTowerId Nothing Nothing = Nothing
    chooseTowerId Nothing (Just e) = Just (equipmentTowerId e)
    chooseTowerId (Just tid) Nothing = Just tid
    chooseTowerId (Just tid) (Just e) = Just (equipmentTowerId e)
    towerSettings = FieldSettings
      { fsLabel = "Tower"
      , fsTooltip = Nothing
      , fsId = Just "tower"
      , fsName = Just "tower"
      , fsAttrs =
        [ ("class", "form-select") ]
      }
    nameSettings = FieldSettings
      { fsLabel = "Equipment Name"
      , fsTooltip = Nothing
      , fsId = Just "name"
      , fsName = Just "name"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "eg: bcn-mtik-ccr")
        ]
      }
    typeSettings = FieldSettings
      { fsLabel = "Type"
      , fsTooltip = Nothing
      , fsId = Just "type"
      , fsName = Just "type"
      , fsAttrs =
        [ ("class", "form-select") ]
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
    softwareVersionSettings = FieldSettings
      { fsLabel = "Software Version"
      , fsTooltip = Nothing
      , fsId = Just "swv"
      , fsName = Just "swv"
      , fsAttrs =
        [ ("class", "form-control")
        ]
      }
    createdAtSettings = FieldSettings
      { fsLabel = "Install Date"
      , fsTooltip = Nothing
      , fsId = Just "install"
      , fsName = Just "install"
      , fsAttrs =
        [ ("class", "form-control") ]
      }
