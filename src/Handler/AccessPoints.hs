{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module Handler.AccessPoints where

import Import hiding (Value)
import Yesod.Form.Bootstrap4 (BootstrapFormLayout (..), renderBootstrap4)
import Database.Esqueleto.Experimental as E

getAccessPointNames :: DB [(Value AccessPointId, Value Text)]
getAccessPointNames = select $ do
  aps <- from $ table @AccessPoint
  pure (aps ^. AccessPointId, aps ^. AccessPointName)

getAPNamesFor :: TowerId -> DB [(Value AccessPointId, Value Text)]
getAPNamesFor towerId = select $ do
  aps <- from $ table @AccessPoint
  where_ (aps ^. AccessPointTowerId E.==. (towerId |> fromSqlKey |> valkey))
  orderBy [desc (aps ^. AccessPointName)]
  pure (aps ^. AccessPointId, aps ^. AccessPointName)

getAllAccessPoints :: DB [Entity AccessPoint]
getAllAccessPoints = selectList [] [Asc AccessPointName]

getAccessPointsR :: Handler Html
getAccessPointsR = do
  allAPs <- runDB getAllAccessPoints
  defaultLayout $ do
    setTitle "Access Points"
    [whamlet|
<h1 class="display-1 text-center">Access Points
<table class="table table-striped table-responsive mt-3 mb-3">
  <thead>
    <tr>Name
    <tr>Height
  <tbody>
    $forall Entity apId ap <- allAPs
      <tr>
        <td>#{accessPointName ap}
        $maybe height <- accessPointHeight ap
          <td>#{height}
        $nothing
          <td>
            |]
