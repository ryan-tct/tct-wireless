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
module Handler.Towers where

import Import hiding (Value)
import Yesod.Form.Bootstrap5 (BootstrapFormLayout (..)
                             , renderBootstrap5
                             , BootstrapGridOptions(..)
                             )
import Helper.Model hiding ((==.))
import Helper.Html
import DoubleLayout
import Text.Read (read)
import Handler.Towers.TowerTypes
import Handler.Comment (commentModalWidget, postCommentR)

getAllTowers :: DB [Entity Tower]
getAllTowers = selectList [] [Asc TowerName]

getTowersR :: Handler Html
getTowersR = do
  doubleLayout $ do
    setTitle "Towers"
    $(widgetFile "towers/towers")

postTowersR :: Handler Html
postTowersR = do
  ((result, _), _) <- runFormPost (towerForm Nothing)
  case result of
    FormSuccess t -> do
      _ <- runDB $ insert t
      setMessage "Tower created."
      redirect TowersR
-- TODO: FIXME
    _ -> doubleLayout [whamlet|Someting went wrong!|]

getTowerAPsR :: TowerId -> Handler Html
getTowerAPsR tId = do
  t <- runDB $ get404 tId
  apNames <- runDB $ getAPNamesFor tId
  doubleLayout $ do
    setTitle $ "APs on " <> toHtml (towerName t)
    $(widgetFile "towers/towerAPs")

getTowerR :: TowerId -> Handler Html
getTowerR tId = do
  t <- runDB $ get404 tId
  doubleLayout $ do
    setTitle "Tower"
    $(widgetFile "towers/tower")
  where --TODO: Make these global?
    towerWidget t = do
      $(widgetFile "towers/towerTable")
    apWidget = do
      apNames <- handlerToWidget $ runDB $ getAPNamesFor tId
      $(widgetFile "towers/apTable")

editTowerWidget :: Maybe (TowerId, Tower) -> Widget
editTowerWidget mp = do
  let (actionR, mt) = case mp of
        Nothing -> (TowersR, Nothing)
        Just (tId, t) -> (TowerR tId, Just t)
  (widget, enctype) <- handlerToWidget $ generateFormPost (towerForm mt)
  $(widgetFile "towers/towerEdit")

towersTableWidget :: Widget
towersTableWidget = do
  allTowers <- handlerToWidget $ runDB getAllTowers
  (widget, enctype) <- handlerToWidget $ generateFormPost (towerForm Nothing)
  $(widgetFile "towers/towersTable")

getTowerType :: TowerTypeId -> Widget
getTowerType ttId = do
  tt <- handlerToWidget $ runDB $ get404 ttId
  [whamlet|<a href=@{TowerTypeR ttId}>#{towerTypeName tt}|]

postTowerR :: TowerId -> Handler Html
postTowerR tId = do
  ((result, _), _) <- runFormPost (towerForm Nothing)
  action <- lookupPostParam "action"
  case (result, action) of
    (FormSuccess t, Just "save") -> do
      runDB $ replace tId t
      setMessage "Updated tower."
      redirect $ TowerR tId
    (FormSuccess _, Just "discard-changes") -> do
      setMessage "No changes made."
      redirect $ TowerR tId
    (FormSuccess _, Just "delete") -> do
      runDB $ delete tId
      setMessage "Deleted tower."
      redirect TowersR
    _ -> doubleLayout $ do
      setMessage "Error editing tower."
      redirect $ TowerR tId

towerForm :: Maybe Tower -> Form Tower
towerForm mt = renderBootstrap5 bootstrapH $ Tower
  <$> areq textField nameSettings (towerName <$> mt)
  <*> areq textField shortSettings (towerShortName <$> mt)
  <*> aopt doubleField latSettings (towerLatitude <$> mt)
  <*> aopt doubleField longSettings (towerLongitude <$> mt)
  <*> aopt textField addressSettings (towerAddress <$> mt)
  <*> aopt intField heightSettings (towerHeight <$> mt)
  <*> aopt textField baseDimSettings (towerBaseDimensions <$> mt)
  <*> aopt (selectField towerTypes) typeSettings (towerTowerTypeId <$> mt)
--  <*> aopt textField typeSettings (towerTowerTypeId <$> mt)
  <*> aopt textField buildingDimSettings (towerBuildingDimensions <$> mt)
  <*> aopt textAreaField accessInfoSettings (towerAccessInfo <$> mt)
  <*> aopt textAreaField leaseInfoSettings (towerLeaseInfo <$> mt)
  <*> aopt textAreaField powerCompanyInfoSettings (towerPowerCompanyInfo <$> mt)
  <*> areq dayField installDateSettings (towerCreatedAt <$> mt)
  <*> (getCurrentTime |> liftIO |> lift)
  where
    towerTypes = do
      towerTypeEntities <- runDB $ selectList [] [Asc TowerTypeName]
      optionsPairs $ map (\tt -> (towerTypeName $ entityVal tt, entityKey tt)) towerTypeEntities
    nameSettings = FieldSettings
      { fsLabel = "Tower Name"
      , fsTooltip = Nothing
      , fsId = Just "name"
      , fsName = Just "name"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "eg: Beacon")
        ]
      }
    shortSettings = FieldSettings
      { fsLabel = "Tower Short Name"
      , fsTooltip = Nothing
      , fsId = Just "short"
      , fsName = Just "short"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "eg: bcn")
        ]
      }
    latSettings = FieldSettings
      { fsLabel = "Latitude"
      , fsTooltip = Nothing
      , fsId = Just "latitude"
      , fsName = Just "latitude"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "In decimal form, eg: 44.522622")
        ]
      }
    longSettings = FieldSettings
      { fsLabel = "Longitude"
      , fsTooltip = Nothing
      , fsId = Just "longitude"
      , fsName = Just "longitude"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "In decimal form, eg: -109.008959 ")
        ]
      }
    addressSettings = FieldSettings
      { fsLabel = "Address"
      , fsTooltip = Nothing
      , fsId = Just "address"
      , fsName = Just "address"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "eg: 104 Beacon Hill Rd, Cody, WY")
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
    baseDimSettings = FieldSettings
      { fsLabel = "Base Dimensions"
      , fsTooltip = Nothing
      , fsId = Just "base-dimensions"
      , fsName = Just "base-dimensions"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "Text, eg: 18x18 feet")
        ]
      }
    typeSettings = FieldSettings
      { fsLabel = "Tower Type"
      , fsTooltip = Nothing
      , fsId = Just "tower-type"
      , fsName = Just "tower-type"
      , fsAttrs =
        [ ("class", "form-select") ]
      }
    buildingDimSettings = FieldSettings
      { fsLabel = "Building Dimensions"
      , fsTooltip = Nothing
      , fsId = Just "building-dimensions"
      , fsName = Just "building-dimensions"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "Text, eg: 18x18 feet")
        ]
      }
    accessInfoSettings = FieldSettings
      { fsLabel = "Access Information"
      , fsTooltip = Nothing
      , fsId = Just "access-info"
      , fsName = Just "access-info"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "eg: Side-by-side is needed. Key to gate is at Cody office.")
        ]
      }
    leaseInfoSettings = FieldSettings
      { fsLabel = "Lease Information"
      , fsTooltip = Nothing
      , fsId = Just "lease-info"
      , fsName = Just "lease-info"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "eg: Tower owned by TCT, site leased from Unided States Bureau of Land Management.")
        ]
      }
    powerCompanyInfoSettings = FieldSettings
      { fsLabel = "Power Company Information"
      , fsTooltip = Nothing
      , fsId = Just "power-company-info"
      , fsName = Just "power-company-info"
      , fsAttrs =
        [ ("class", "form-control")
        , ("placeholder", "eg: Power from Rocky Mountain Power, contact Jim 123-123-1234.")
        ]
      }
    installDateSettings = FieldSettings
      { fsLabel = "Install Date"
      , fsTooltip = Nothing
      , fsId = Just "install-date"
      , fsName = Just "install-date"
      , fsAttrs =
        [ ("class", "form-control")
        ]
      }

getAllTowerComments :: TowerId -> DB [Entity TowerComment]
getAllTowerComments tId = selectList [TowerCommentTowerId ==. tId] [Desc TowerCommentUpdatedAt]

commentsList :: TowerId -> Widget
commentsList tId = do
  allTowerComments <- handlerToWidget $ runDB $ getAllTowerComments tId
  let allComments = map fromTowerComment allTowerComments
  (formWidget, formEncType) <- handlerToWidget $ generateFormPost $ towerCommentForm tId
  $(widgetFile "comments/commentsList")
  where
    fromTowerComment :: Entity TowerComment -> Comment
    fromTowerComment (Entity key tc) = Comment
      { commentMessage = towerCommentMessage tc
      , commentUpdatedAt = towerCommentUpdatedAt tc
      }

data PreTowerComment = PreTowerComment
  { towerId :: TowerId
  , message :: Text
  , updatedAt :: UTCTime
  }

towerCommentForm :: TowerId -> Form PreTowerComment
towerCommentForm tId = renderBootstrap5 bootstrapH $ PreTowerComment
  <$> pure tId
  <*> areq textField messageSettings Nothing
  <*> (getCurrentTime |> liftIO |> lift)
  where messageSettings = FieldSettings
          { fsLabel = "Message"
          , fsTooltip = Nothing
          , fsId = Just "message"
          , fsName = Just "message"
          , fsAttrs =
            [ ("class", "form-control")
            ]
          }

