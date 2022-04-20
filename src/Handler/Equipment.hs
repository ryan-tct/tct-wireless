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
module Handler.Equipment where

import Import hiding (Value)
import Yesod.Form.Bootstrap5 (BootstrapFormLayout (..)
                             , renderBootstrap5
                             , BootstrapGridOptions(..)
                             )
import Helper.Model hiding ((==.))
import Helper.Html
import DoubleLayout

getAllEquipment :: DB [Entity Equipment]
getAllEquipment = selectList [] [Desc EquipmentName]

getEquipmentOn :: TowerId -> DB [Entity Equipment]
getEquipmentOn tId = selectList [EquipmentTowerId ==. tId] [Desc EquipmentName]

equipmentListWidget :: Foldable t => t (Entity Equipment) -> Widget
equipmentListWidget allEquipment = $(widgetFile "equipment/equipmentsList")

getEquipmentsR :: Handler Html
getEquipmentsR = do
  doubleLayout $ do
    setTitle "Equipment"
    $(widgetFile "equipment/equipmentsView")

postEquipmentsR :: Handler Html
postEquipmentsR = undefined
