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
module Handler.Towers where

import Import
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)
import Flow
import Text.Read (read)

getAllTowers :: DB [Entity Tower]
getAllTowers = selectList [] [Asc TowerName]

getTowersR :: Handler Html
getTowersR = do
  allTowers <- runDB  getAllTowers
  defaultLayout $ do
    setTitle "Towers"
    $(widgetFile "towers/towers")

getTowerR :: TowerId -> Handler Html
getTowerR tId = do
  t <- runDB $ get404 tId
  (widget, enctype) <- generateFormPost (towerForm (Just t))
  defaultLayout $ do
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
    _ -> defaultLayout $ do
      setMessage "Error editing tower."
      redirect $ TowerR tId

towerForm :: Maybe Tower -> Form Tower
towerForm mt = renderBootstrap4 BootstrapBasicForm $ Tower
  <$> areq textField nameSettings (towerName <$> mt)
  <*> areq textField "shortName" (towerShortName <$> mt)
  <*> aopt doubleField "latitude" (towerLatitude <$> mt)
  <*> aopt doubleField "longitude" (towerLongitude <$> mt)
  <*> aopt textField "address" (towerAddress <$> mt)
  <*> aopt intField "height" (towerHeight <$> mt)
  <*> aopt textField "baseDimensions" (towerBaseDimensions <$> mt)
  <*> aopt textField "type" (towerType <$> mt)
  <*> aopt textField "buildingDimensions" (towerBuildingDimensions <$> mt)
  <*> aopt textField "accessInfo" (towerAccessInfo <$> mt)
  <*> aopt textField "leasInfo" (towerLeaseInfo <$> mt)
  <*> aopt textAreaField  "powerCompanyInfo" (towerPowerCompanyInfo <$> mt)
  <*> areq dayField "createdAt" (towerCreatedAt <$> mt)
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
    textAreaField :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Text
    textAreaField = Field
      { fieldParse = parseHelper $ Right
      , fieldView = \theId name attrs val isReq ->
          [whamlet|
$newline never
<textarea id="#{theId}" name="#{name}" *{attrs} type="text" :isReq:required>#{either id id val}
          |]
      , fieldEnctype = UrlEncoded
      }


-- testTower :: Tower
-- testTower = Tower { towerName = "name"
--                   , towerShortName = "short_name"
--                   , towerLatitude = Nothing
--                   , towerLongitude = Nothing
--                   , towerAddress = Nothing
--                   , towerHeight = Nothing
--                   , towerBaseDimensions = Nothing
--                   , towerType = Nothing
--                   , towerBuildingDimensions = Nothing
--                   , towerAccessInfo = Nothing
--                   , towerLeaseInfo = Nothing
--                   , towerPowerCompanyInfo = Nothing
--                   , towerCreatedAt = (read "2022-04-01 18:32:28.80049733 UTC" :: UTCTime)
--                   , towerUpdatedAt = (read "2022-04-01 18:32:28.80049733 UTC" :: UTCTime)
--                   }
