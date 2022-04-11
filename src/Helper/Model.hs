{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module Helper.Model
  ( module E
  , getTowerNames
  , getAccessPointNames
  , getAPNamesFor
  , getAccessPointTypeNames
  , unValueSwap
--  , keyToText
  ) where

import Import hiding (Value)
import Database.Esqueleto.Experimental as E hiding(delete, isNothing)
import qualified Data.Text as T

getTowerNames :: DB [(Value TowerId, Value Text)]
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

getAccessPointTypeNames :: DB [(Value AccessPointTypeId, Value Text)]
getAccessPointTypeNames = select $ do
  apts <- from $ table @AccessPointType
  pure (apts ^. AccessPointTypeId, apts ^. AccessPointTypeName)

unValueSwap vs = map (\(Value key, Value text) -> (text, key)) vs

--keyToText key = key |>fromSqlKey |> show |> T.pack
