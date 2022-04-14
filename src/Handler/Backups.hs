{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Handler.Backups where

import Import hiding (Value)
import Yesod.Form.Bootstrap5 (BootstrapFormLayout (..)
                             , renderBootstrap5
                             , BootstrapGridOptions(..)
                             )
import DoubleLayout
import Helper.Model
import Helper.Html

import Database.Esqueleto.Experimental as E hiding(delete, isNothing)
import Data.Conduit.Binary
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

getAllBackups :: DB [(Single BackupId,Single UTCTime,Single Text)]
getAllBackups = rawSql "SELECT backupid,updated_at,filename FROM aps_backups ORDER BY updated_at DESC" []

getBackupR :: BackupId -> Handler TypedContent
getBackupR bId = do
 b <- runDB $ get404 bId
 addHeader "Content-Disposition" $ T.concat
   [ "attachment; filename=\"", (backupFilename b), "\""]
 sendResponse (T.encodeUtf8 (backupContentType b), toContent (backupPayload b))

getBackupsR :: Handler Html
getBackupsR = do
  allBackups <- runDB getAllBackups
  doubleLayout $ do
    setTitle "Backups"
    $(widgetFile "backups/backupsTable")

postBackupsR :: Handler Html
postBackupsR = undefined

