{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes #-}
module Handler.AccessPoints.AccessPointTypes where

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

getAccessPointTypesR :: Handler Html
getAccessPointTypesR = do
  doubleLayout $ do
    setTitle "AP Types"
    $(widgetFile "accessPoints/accessPointTypes")
    
postAccessPointTypesR :: Handler Html
postAccessPointTypesR = do
  ((result, _), _) <- runFormPost $ accessPointTypeForm Nothing
  case result of
    FormSuccess apt -> do 
      runDB $ insert apt
      setMessage "Access Point Type created."
      redirect AccessPointTypesR
    _ -> doubleLayout [whamlet|Something went wrong!|]

getAccessPointTypeR :: AccessPointTypeId -> Handler Html
getAccessPointTypeR aptId = do
  apt <- runDB $ get404 aptId
  doubleLayout $ do
    setTitle "Acess Point Type"
    $(widgetFile "accessPoints/accessPointType")
  where
    aptTableWidget apt = do
      (widget, enctype) <- handlerToWidget $ generateFormPost (accessPointTypeForm (Just apt))
      $(widgetFile "accessPoints/accessPointTypeTable")

postAccessPointTypeR :: AccessPointTypeId -> Handler Html
postAccessPointTypeR aptId = do
  ((result, _), _) <- runFormPost $ accessPointTypeForm Nothing
  action <- lookupPostParam "action"
  case (result, action) of
    (FormSuccess apt, Just "save") -> do
      runDB $ replace aptId apt
      setMessage "Updated access point type."
      redirect $ AccessPointTypeR aptId
    (FormSuccess _, Just "discard-changes") -> do
      setMessage "No changes made."
      redirect $ AccessPointTypeR aptId
    (FormSuccess _, Just "delete") -> do
      runDB $ delete aptId
      setMessage "Deleted access point type"
      redirect AccessPointTypesR
    _ -> doubleLayout $ do
      setMessage "Error editing access point type."
      redirect $ AccessPointTypeR aptId

accessPointTypeForm :: Maybe AccessPointType -> Form AccessPointType
accessPointTypeForm mapt = renderBootstrap5 bootstrapH $ AccessPointType
  <$> areq textField nameSettings (accessPointTypeName <$> mapt)
  where
    bootstrapH = BootstrapHorizontalForm
      { bflLabelOffset = ColSm 0
      , bflLabelSize = ColSm 4
      , bflInputOffset = ColSm 0
      , bflInputSize = ColSm 8
      }
    nameSettings = FieldSettings
          { fsLabel = "Access Point Type"
          , fsTooltip = Nothing
          , fsId = Just "name"
          , fsName = Just "name"
          , fsAttrs =
            [ ("class", "form-control")
            , ("placeholder", "eg: PMP450m")
            ]
          }

getAllAPTypes :: DB [Entity AccessPointType]
getAllAPTypes = selectList [] [Asc AccessPointTypeName]

getAllDocsFor :: AccessPointTypeId -> DB [Entity AccessPointTypeDoc]
getAllDocsFor aptId = selectList [AccessPointTypeDocAptypeId ==. aptId]
                                 [Desc AccessPointTypeDocUpdatedAt]

apTypesTableWidget :: Widget
apTypesTableWidget = do
  allAPTypes <- handlerToWidget $ runDB getAllAPTypes
  (widget, enctype) <- handlerToWidget $ generateFormPost (accessPointTypeForm Nothing)
  $(widgetFile "accessPoints/accessPointTypesTable")

editAPTWidget :: Maybe (AccessPointTypeId, AccessPointType) -> Widget
editAPTWidget mp = do
  let (actionR, mAPT) = case mp of
        Nothing -> (AccessPointTypesR, Nothing)
        Just (aptId, apt) -> (AccessPointTypeR aptId, Just apt)
  (widget, enctype) <- handlerToWidget $ generateFormPost (accessPointTypeForm mAPT)
  $(widgetFile "accessPoints/accessPointTypeEdit")

aptDocWidget :: AccessPointTypeId -> Widget
aptDocWidget aptId = do
  allDocs <- handlerToWidget $ runDB $ getAllDocsFor aptId
  (docWidget, docEnctype) <- handlerToWidget $ generateFormPost docForm 
  $(widgetFile "accessPoints/accessPointTypeDocTable")

postAPTDocR :: AccessPointTypeId -> Handler Html
postAPTDocR aptId = do
  ((result, _), _) <- runFormPost docForm
  case result of
    FormSuccess f -> do
      bytes <- connect (f |> fileInfo |> fileSource) sinkLbs
      let
        fn = fileName $ fileInfo f
        ua = updatedAt f
        contType = fileContentType $ fileInfo f
      docId <- runDB $ insert $ FileStore fn contType (bytes |> toStrict) ua
      _ <- runDB $ rawExecute "INSERT INTO accesspointtype_documentation (accesspointtypeid, documentationid) values (?,?)" [PersistText (keyToText aptId), PersistText (keyToText docId)]
      setMessage "Documentation saved."
      redirect $ AccessPointTypeR aptId
    _ -> doubleLayout [whamlet|Something went wrong!|]
