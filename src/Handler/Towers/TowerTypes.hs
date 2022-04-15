{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes       #-}
module Handler.Towers.TowerTypes where

import Import
import Yesod.Form.Bootstrap5 (BootstrapFormLayout (..)
                             , renderBootstrap5
                             , BootstrapGridOptions(..)
                             )
--import Helper.Html
import DoubleLayout

getTowerTypesR :: Handler Html
getTowerTypesR = do
  doubleLayout $ do
    setTitle "Tower Types"
    $(widgetFile "towers/towerTypes")
    
postTowerTypesR :: Handler Html
postTowerTypesR = do
  ((result, _), _) <- runFormPost (towerTypeForm Nothing)
  case result of
    FormSuccess t -> do 
      _ <- runDB $ insert t
      setMessage "Tower Type created."
      redirect TowerTypesR
-- TODO: FIXME
    _ -> doubleLayout [whamlet|Something went wrong!|]

getTowerTypeR :: TowerTypeId -> Handler Html
getTowerTypeR tId = do
  t <- runDB $ get404 tId
  (widget, enctype) <- generateFormPost (towerTypeForm (Just t))
  doubleLayout $ do
    setTitle "Tower Type"
    $(widgetFile "towers/towerType")

postTowerTypeR :: TowerTypeId -> Handler Html
postTowerTypeR tId = do
  ((result, _), _) <- runFormPost (towerTypeForm Nothing)
  action <- lookupPostParam "action"
  case (result, action) of
    (FormSuccess t, Just "save") -> do
      runDB $ replace tId t
      setMessage "Updated Tower Type."
      redirect $ TowerTypeR tId
    (FormSuccess _, Just "discard-changes") -> do
      setMessage "No changes made."
      redirect $ TowerTypeR tId
    (FormSuccess _, Just "delete") -> do
      runDB $ delete tId
      setMessage "Deleted Tower Type."
      redirect TowerTypesR
    _ -> doubleLayout $ do
      setMessage "Error editing tower type."
      redirect $ TowerTypeR tId

towerTypeForm :: Maybe TowerType -> Form TowerType
towerTypeForm mt = renderBootstrap5 bootstrapH $ TowerType
  <$> areq textField nameSettings (towerTypeName <$> mt)
  where
    bootstrapH = BootstrapHorizontalForm
      { bflLabelOffset = ColSm 0
      , bflLabelSize = ColSm 4
      , bflInputOffset = ColSm 0
      , bflInputSize = ColSm 8
      }
    nameSettings = FieldSettings
          { fsLabel = "Tower Type"
          , fsTooltip = Nothing
          , fsId = Just "name"
          , fsName = Just "name"
          , fsAttrs =
            [ ("class", "form-control")
            , ("placeholder", "eg: Mono Pole")
            ]
          }

getAllTowerTypes :: DB [Entity TowerType]
getAllTowerTypes = selectList [] [Asc TowerTypeName]

towerTypesTableWidget :: Widget
towerTypesTableWidget = do
  allTowerTypes <- handlerToWidget $ runDB getAllTowerTypes
  (widget, enctype) <- handlerToWidget $ generateFormPost (towerTypeForm Nothing)
  $(widgetFile "towers/towerTypesTable")

editTowerTypeWidget :: Maybe (TowerTypeId, TowerType) -> Widget
editTowerTypeWidget mp = do
  let (actionR, mtt) = case mp of
        Nothing -> (TowerTypesR, Nothing)
        Just (ttId, tt) -> (TowerTypeR ttId, Just tt)
  (widget, enctype) <- handlerToWidget $ generateFormPost (towerTypeForm mtt)
  $(widgetFile "towers/towerTypeEdit")
  
