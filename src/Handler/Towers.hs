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
module Handler.Towers where

import Import -- hiding (Value)
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)
import Model.Helper
import DoubleLayout
import Text.Read (read)

getAllTowers :: DB [Entity Tower]
getAllTowers = selectList [] [Asc TowerName]

getTowersR :: Handler Html
getTowersR = do
  allTowers <- runDB  getAllTowers
  (widget, enctype) <- generateFormPost (towerForm Nothing)
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

getTowerR :: TowerId -> Handler Html
getTowerR tId = do
  t <- runDB $ get404 tId
  apNames <- runDB $ getAPNamesFor tId
  (widget, enctype) <- generateFormPost (towerForm (Just t))
  doubleLayout $ do
    setTitle "Tower"
    $(widgetFile "towers/tower")

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
towerForm mt = renderBootstrap4 BootstrapBasicForm $ Tower
  <$> areq textField nameSettings (towerName <$> mt)
  <*> areq textField shortSettings (towerShortName <$> mt)
  <*> aopt doubleField latSettings (towerLatitude <$> mt)
  <*> aopt doubleField longSettings (towerLongitude <$> mt)
  <*> aopt textField addressSettings (towerAddress <$> mt)
  <*> aopt intField heightSettings (towerHeight <$> mt)
  <*> aopt textField baseDimSettings (towerBaseDimensions <$> mt)
  <*> aopt textField typeSettings (towerType <$> mt)
  <*> aopt textField buildingDimSettings (towerBuildingDimensions <$> mt)
  <*> aopt textAreaField accessInfoSettings (towerAccessInfo <$> mt)
  <*> aopt textAreaField leaseInfoSettings (towerLeaseInfo <$> mt)
  <*> aopt textAreaField powerCompanyInfoSettings (towerPowerCompanyInfo <$> mt)
  <*> areq dayField installDateSettings (towerCreatedAt <$> mt)
  <*> (getCurrentTime |> liftIO |> lift)
  where
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
        [ ("class", "form-control")
        , ("placeholder", "Text, eg: Monopole")
        ]
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


myTestTower :: Tower
myTestTower = Tower {towerName = "Beacon", towerShortName = "bcn", towerLatitude = Just 44.522622, towerLongitude = Just (-109.008959), towerAddress = Just "104 Beacon Hill Rd, Cody, WY", towerHeight = Just 20, towerBaseDimensions = Just "18", towerType = Just "Mono Pole", towerBuildingDimensions = Just "No building on site.", towerAccessInfo = Nothing, towerLeaseInfo = Just "Tower owned by TCT. Site leased from Jim Nicholson: PO Box 3212, Cody, WY 82414.", towerPowerCompanyInfo = Nothing, towerCreatedAt = read "2021-07-28", towerUpdatedAt = read "2022-03-24 18:37:06.449384"}
