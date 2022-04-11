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
module Model.Helper
  ( module E
  , getTowerNames
  , getAccessPointNames
  , getAPNamesFor
  ) where

import Import hiding (Value)
import Database.Esqueleto.Experimental as E hiding(delete, isNothing)

getTowerNames :: DB [(Value (Key Tower), Value Text)]
getTowerNames = select $ do
  towers <- from $ table @Tower
  orderBy [asc (towers ^. TowerName)]
  pure (towers ^. TowerId, towers ^. TowerName)

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
