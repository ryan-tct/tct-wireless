{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes #-}
module Handler.Equipment.EquipmentTypes where

import Import
import Yesod.Form.Bootstrap5 (BootstrapFormLayout (..)
                             , renderBootstrap5
                             , BootstrapGridOptions(..)
                             )
import Helper.Html
import DoubleLayout
import Data.Conduit.Binary
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Helper.Model hiding((==.))
import Handler.Documentation
import Helper.Modals

getEquipmentTypesR :: Handler Html
getEquipmentTypesR = do
  allEquipmentTypes <- runDB getAllEquipmentTypes
  let
    postR = EquipmentTypesR
    discardR = EquipmentTypesR
  doubleLayout $ do
    setTitle "Equipment Types"
    $(widgetFile "equipment/equipmentTypesView")

postEquipmentTypesR :: Handler Html
postEquipmentTypesR = do
  ((result, _), _) <- runFormPost $ equipmentTypeForm Nothing
  case result of
    FormSuccess et -> do
      runDB $ insert et
      setMessage "Equipment Type created."
      redirect EquipmentTypesR
    _ -> doubleLayout [whamlet|Something went wrong!|]

getEquipmentTypeR :: EquipmentTypeId -> Handler Html
getEquipmentTypeR etId = do
  et <- runDB $ get404 etId
  allDocs <- runDB $ getEqTypeDocsFor etId
  doubleLayout $ do
    setTitle "Equipment Type"
    equipmentTypeViewWidget et etId allDocs (EquipmentTypeR etId) (EquipmentTypeR etId)

postEquipmentTypeR :: EquipmentTypeId -> Handler Html
postEquipmentTypeR etId = do
  ((result, _), _) <- runFormPost $ equipmentTypeForm Nothing
  action <- lookupPostParam "action"
  case (result, action) of
    (FormSuccess et, Just "save") -> do
      runDB $ replace etId et
      setMessage "Updated equipment type."
      redirect $ EquipmentTypeR etId
    (FormSuccess _, Just "delete") -> do
      runDB $ delete etId
      setMessage "Delete equipment type."
      redirect EquipmentTypesR
    _ -> doubleLayout $ do
      setMessage "Error editing equipment type."
      redirect $ EquipmentTypeR etId

postETDocR :: EquipmentTypeId -> Handler Html
postETDocR etId = do
  ((result, _), _) <- runFormPost docForm
  case result of
    FormSuccess f -> do
      bytes <- connect (f |> fileInfo |> fileSource) sinkLbs
      let
        fn = fileName $ fileInfo f
        ua = updatedAt f
        contType = fileContentType $ fileInfo f
      docId <- runDB $ insert $ FileStore fn contType (bytes |> toStrict) ua
      _ <- runDB $ rawExecute "INSERT INTO equipmenttype_documentation (equipmenttypeid, documentationid) values (?,?)" [PersistText (keyToText etId), PersistText (keyToText docId)]
      setMessage "Documentation saved."
      redirect $ EquipmentTypeR etId
    _ -> doubleLayout [whamlet|Something went wrong!|]

getAllEquipmentTypes :: DB [Entity EquipmentType]
getAllEquipmentTypes = selectList [] [Asc EquipmentTypeName]

getEqTypeDocsFor :: EquipmentTypeId -> DB [Entity EquipmentTypeDoc]
getEqTypeDocsFor etId = selectList [EquipmentTypeDocEquipmentTypeId ==. etId]
                                   [Desc EquipmentTypeDocUpdatedAt]

--equipmentTypeViewWidget :: EquipmentType -> Route App -> Route App -> Widget
equipmentTypeViewWidget et etId allDocs postR discardR = $(widgetFile "equipment/equipmentTypeView")

equipmentTypesTableWidget :: Foldable t => t (Entity EquipmentType) -> Widget
equipmentTypesTableWidget eets = $(widgetFile "equipment/equipmentTypesTable")

equipmentTypeTableWidget :: EquipmentType -> Widget
equipmentTypeTableWidget et = $(widgetFile "equipment/equipmentTypeTable")

equipmentTypesFormWidget :: Maybe EquipmentType -> Route App -> Route App -> Widget
equipmentTypesFormWidget me postR discardR = do
  (formWidget, formEncType) <- handlerToWidget $ generateFormPost $ equipmentTypeForm me
  $(widgetFile "equipment/equipmentTypesForm")

-- equipmentTypesModalWidget :: Maybe EquipmentType -> Route App -> Route App -> Widget
-- equipmentTypesModalWidget met postR discardR = $(widgetFile "equipment/equipmentTypesModal")

eqTypeDocTableWidget :: Foldable t => t (Entity EquipmentTypeDoc) -> Widget
eqTypeDocTableWidget eeqTypeDocs = do
  [whamlet|
<table class="table table-striped table-responsive mb-3 mt-3">
  <thead>
    <tr>
      <th>Name
      <th>Created At
  <tbody>
    $forall Entity docId doc <- eeqTypeDocs
      <tr>
        <td><a href=@{FileStoreR $ equipmentTypeDocFilestoreId doc}>#{equipmentTypeDocFilename doc}
        <td>#{show $ equipmentTypeDocUpdatedAt doc}
          |]
  -- $(widgetFile "equipment/equipmentTypeDocTable")

eqTypeDocModalWidget :: EquipmentTypeId -> Widget
eqTypeDocModalWidget etId = do
  (docWidget, docEnctype) <- handlerToWidget $ generateFormPost docForm
  [whamlet|
<div class="mt-3 mb-3">
  <button type="button" class="btn btn-primary" data-bs-toggle="modal" data-bs-target="#newUploadModal">Upload Documentation
<div class="modal fade" id="newUploadModal" tabindex="-1" aria-labelledby="nameUploadModalLabel" aria-hidden="true">
  <div class="modal-dialog">
    <div class="modal-content">
      <div class="modal-header">
        <h5 class="modal-title" id="uploadModalLabel">Upload Documentation</h5>
        <button type="button" class="btn-close" data-bs-dismiss="modal" aria-label="Close"></button>
      <div class="modal-body">
        <form class="form mt-3 mb-3" method=POST action=@{ETDocR etId} enctype=#{docEnctype}>
          ^{docWidget}
          <div class="form mt-3 mb-3">
            <button class="btn btn-warning" type="button" data-bs-dismiss="modal"">Close
            <button class="btn btn-primary" type="submit" name="action" value="save">Save Documentation
          |]
    
equipmentTypeForm :: Maybe EquipmentType -> Form EquipmentType
equipmentTypeForm met = renderBootstrap5 bootstrapH $ EquipmentType
  <$> areq textField nameSettings (equipmentTypeName <$> met)
  where
    bootstrapH = BootstrapHorizontalForm
      { bflLabelOffset = ColSm 0
      , bflLabelSize = ColSm 4
      , bflInputOffset = ColSm 0
      , bflInputSize = ColSm 8
      }
    nameSettings = FieldSettings
          { fsLabel = "Equipment Type"
          , fsTooltip = Nothing
          , fsId = Just "name"
          , fsName = Just "name"
          , fsAttrs =
            [ ("class", "form-control")
            , ("placeholder", "eg: MikroTik CCR1016-12G")
            ]
          }
          
