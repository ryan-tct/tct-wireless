{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes #-}
module Helper.Modals where

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

generalFormModal :: Text -> Widget -> Widget
generalFormModal formTitle formWidget = $(widgetFile "modals/generalFormModal")
