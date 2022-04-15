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
import Data.Conduit.Binary
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

getAllBackups :: DB [(Single FileStoreId,Single UTCTime,Single Text)]
getAllBackups = rawSql "SELECT backupid,updated_at,filename FROM aps_backups ORDER BY updated_at DESC" []

getBackupR :: FileStoreId -> Handler TypedContent
getBackupR bId = do
 b <- runDB $ get404 bId
 addHeader "Content-Disposition" $ T.concat
   [ "attachment; filename=\"", (fileStoreFilename b), "\""]
 sendResponse (T.encodeUtf8 (fileStoreContentType b), toContent (fileStorePayload b))

getBackupsR :: Handler Html
getBackupsR = do
  allBackups <- runDB getAllBackups
  doubleLayout $ do
    setTitle "Backups"
    $(widgetFile "backups/backupsTable")

postBackupsR :: Handler Html
postBackupsR = undefined

