{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications #-}
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
  doubleLayout $ do
    setTitle "Access Point"
    $(widgetFile "accessPoints/accessPoint")

postAccessPointR :: AccessPointId -> Handler Html
postAccessPointR apId = undefined
