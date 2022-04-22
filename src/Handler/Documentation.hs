{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes #-}
module Handler.Documentation where

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

data DocForm = DocForm
  { fileInfo :: FileInfo
  , updatedAt :: UTCTime
  }

docForm :: Form DocForm
docForm = renderBootstrap5 bootstrapH $ DocForm
  <$> fileAFormReq "Choose a file"
  <*> (getCurrentTime |> liftIO |> lift)

